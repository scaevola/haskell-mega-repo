{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Types.Ctx (
    Ctx (..),
    newCtx,
    ctxApplyCmd,
    ctxGetCRandom,
    -- * Helpers
    ctxWithCryptoGen,
    ) where

import Prelude ()
import Futurice.Prelude
import Control.Concurrent.Async (concurrently)
import Control.Concurrent.STM
       (TVar, atomically, modifyTVar', newTVarIO, readTVar, writeTVar)
import Data.Pool                (Pool, createPool, withResource)
import Futurice.CryptoRandom
       (CRandT, CRandom, CryptoGen, CryptoGenError, getCRandom, mkCryptoGen,
       runCRandT)

import qualified Database.PostgreSQL.Simple as Postgres
import qualified FUM
import qualified Data.Map                   as M

import Futurice.App.Checklist.Command
import Futurice.App.Checklist.Logic
import Futurice.App.Checklist.Types

data Ctx = Ctx
    { ctxLogger      :: !Logger
    , ctxManager     :: !Manager
    , ctxWorld       :: TVar World
    , ctxOrigWorld   :: World
    , ctxPostgres    :: Pool Postgres.Connection
    , ctxPRNGs       :: Pool (TVar CryptoGen)
    , ctxMockUser    :: !(Maybe FUM.Login)
    , ctxACL         :: TVar (Map FUM.Login TaskRole)
    }

newCtx
    :: Logger
    -> Postgres.ConnectInfo
    -> Maybe FUM.Login
    -> World
    -> IO Ctx
newCtx logger ci mockUser w = do
    mgr <- newManager tlsManagerSettings
    Ctx logger mgr
        <$> newTVarIO w
        <*> pure w
        <*> createPool (Postgres.connect ci) Postgres.close 1 60 5
        <*> createPool (mkCryptoGen >>= newTVarIO) (\_ -> return()) 1 3600 5
        <*> pure mockUser
        <*> newTVarIO M.empty

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
    => UTCTime -> FUM.Login -> Command Identity -> Ctx -> m ()
ctxApplyCmd now fumuser cmd ctx = do
    liftIO $ atomically $ modifyTVar' (ctxWorld ctx) (applyCommand now fumuser cmd)
    withResource (ctxPostgres ctx) $ \conn ->
        transactCommand conn fumuser cmd
