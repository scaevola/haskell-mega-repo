{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Types.Ctx (
    Ctx (..),
    newCtx,
    ctxApplyCmd,
    ctxGetCRandom,
    -- * Helpers
    ctxWithCryptoGen,
    ctxFetchGroups
    ) where

import Prelude ()
import Futurice.Prelude
import Control.Concurrent.Async (concurrently)
import Control.Concurrent.STM
       (TVar, atomically, modifyTVar', newTVarIO, readTVar, writeTVar)
import Data.Constraint          (Dict (..))
import Data.Pool                (Pool, createPool, withResource)
import Futurice.CryptoRandom
       (CRandT, CRandom, CryptoGen, CryptoGenError, getCRandom, mkCryptoGen,
       runCRandT)

import qualified Database.PostgreSQL.Simple as Postgres
import qualified FUM

import Futurice.App.Checklist.Command
import Futurice.App.Checklist.Logic
import Futurice.App.Checklist.Types

data Ctx = Ctx
    { ctxLogger      :: !Logger
    , ctxWorld       :: TVar World
    , ctxOrigWorld   :: World
    , ctxValidTribes :: Dict HasValidTribes
    , ctxPostgres    :: Pool Postgres.Connection
    , ctxPRNGs       :: Pool (TVar CryptoGen)
    , ctxMockUser    :: !(Maybe FUM.UserName)
    , ctxACL         :: TVar (Map FUM.UserName TaskRole)
    , ctxManager     :: Manager
    }

newCtx
    :: HasValidTribes
    => Logger
    -> Postgres.ConnectInfo
    -> FUM.AuthToken
    -> FUM.BaseUrl
    -> (FUM.GroupName, FUM.GroupName, FUM.GroupName)
    -> Maybe FUM.UserName
    -> World
    -> IO Ctx
newCtx logger ci fumAuthToken fumBaseUrl (itGroupName, hrGroupName, supervisorGroupName) mockUser w = do
    mgr <- newManager tlsManagerSettings
    Ctx logger
        <$> newTVarIO w
        <*> pure w
        <*> pure Dict
        <*> createPool (Postgres.connect ci) Postgres.close 1 60 5
        <*> createPool (mkCryptoGen >>= newTVarIO) (\_ -> return()) 1 3600 5
        <*> pure mockUser
        <*> ctxFetchGroups newTVarIO mgr logger fumAuthToken fumBaseUrl (itGroupName, hrGroupName, supervisorGroupName)
        <*> pure mgr

ctxWithCryptoGen
    :: MonadIO m
    => Ctx -> CRandT CryptoGen CryptoGenError Identity a -> m a
ctxWithCryptoGen ctx m = liftIO $
    withResource (ctxPRNGs ctx) $ \tg -> atomically $ do
        g <- readTVar tg
        (x, g') <- either throwM pure $ runIdentity $ runCRandT m g
        writeTVar tg g'
        pure x

ctxGetCRandom :: (MonadIO m, CRandom a) => Ctx -> m a
ctxGetCRandom ctx = ctxWithCryptoGen ctx getCRandom

ctxApplyCmd
    :: (MonadLog m, MonadBaseControl IO m, MonadIO m)
    => UTCTime -> FUM.UserName -> Command Identity -> Ctx -> m ()
ctxApplyCmd now fumuser cmd ctx = do
    liftIO $ atomically $ modifyTVar' (ctxWorld ctx) (applyCommand now fumuser cmd)
    withResource (ctxPostgres ctx) $ \conn -> do
        transactCommand conn fumuser cmd

ctxFetchGroups
    :: (Map FUM.UserName TaskRole -> IO b)
    -> Manager
    -> Logger
    -> FUM.AuthToken
    -> FUM.BaseUrl
    -> (FUM.GroupName, FUM.GroupName, FUM.GroupName)
    -> IO b
ctxFetchGroups returnAs mgr logger fumAuthToken fumBaseUrl (itGroupName, hrGroupName, supervisorGroupName) = do
    ((itGroup, hrGroup), supervisorGroup) <-
        fetchGroup mgr itGroupName `concurrently`
        fetchGroup mgr hrGroupName `concurrently`
        fetchGroup mgr supervisorGroupName
    returnAs $ toMapOf (folded . ifolded) $
        [ (login, TaskRoleIT) | login <- itGroup ^.. FUM.groupUsers . folded ] ++
        [ (login, TaskRoleHR) | login <- hrGroup ^.. FUM.groupUsers . folded ] ++
        [ (login, TaskRoleSupervisor) | login <- supervisorGroup ^.. FUM.groupUsers . folded ]
  where
      fetchGroup manager n = runLogT "FUM Fetch" logger $ do
          logInfo "Fetching FUM Group" n
          liftIO $ FUM.executeRequest manager fumAuthToken fumBaseUrl (FUM.fumGroupR n)
