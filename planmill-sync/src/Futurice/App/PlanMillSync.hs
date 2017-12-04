{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.PlanMillSync (defaultMain) where

import Control.Applicative       (liftA3)
import Futurice.Integrations
import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Lucid.Foundation (fullRow_, h1_, page_)
import Futurice.Prelude
import Futurice.Servant
import PlanMill.Worker           (workers)
import Prelude ()
import Servant

import Futurice.FUM.MachineAPI (FUM6 (..), fum6)

import Futurice.App.PlanMillSync.Actions
import Futurice.App.PlanMillSync.API
import Futurice.App.PlanMillSync.Config
import Futurice.App.PlanMillSync.Ctx
import Futurice.App.PlanMillSync.IndexPage
import Futurice.App.PlanMillSync.Monad
import Futurice.App.PlanMillSync.Types

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import qualified FUM
import qualified Personio        as P

server :: Ctx -> Server PlanMillSyncAPI
server ctx = indexPageAction ctx
    -- actions
    :<|> addDepartDateAction ctx
    -- TODO: actions to add & remove

-------------------------------------------------------------------------------
-- Indexpage
-------------------------------------------------------------------------------

indexPageAction
    :: Ctx
    -> Maybe FUM.Login
    -> Handler (HtmlPage "index")
indexPageAction ctx mfu = withAuthorisedUser ctx mfu err $ do
    now <- currentTime

    -- fetch data
    (pm, fum, p) <- liftIO $ cachedIO lgr cache 300 () $
        runIntegrations' ctx fetcher

    -- Render
    pure $ indexPage now pm fum p
  where
    err = pure page404

    lgr   = ctxLogger ctx
    cache = ctxCache ctx

page404 :: HtmlPage a
page404 = page_ "PlanMill Sync - Unauthorised" $
    fullRow_ $ do
        h1_ "Unauthorised"
        "Ask IT Team for access rights"

-------------------------------------------------------------------------------
-- Actions
-------------------------------------------------------------------------------

addDepartDateAction :: Ctx -> Maybe FUM.Login -> FUM.Login -> Handler Text
addDepartDateAction ctx mfu login = withAuthorisedUser ctx mfu err $ do
    updateDepartDate ctx login
  where
    err = throwError err403

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------

withAuthorisedUser :: Ctx -> Maybe FUM.Login -> Handler a -> Handler a -> Handler a
withAuthorisedUser ctx mfu err action = do
    now <- currentTime

    -- Access control: TODO cache?
    fus <- liftIO $ runIntegrations mgr lgr now (cfgIntegrationsConfig2 cfg) $
        fum6 $ FUMGroupEmployees (cfgAccessGroup cfg)

    case mfu <|> cfgMockUser cfg of
        Just fu | Set.member fu fus -> action

        -- TODO: log?
        _ -> err
  where
    cfg   = ctxConfig ctx
    lgr   = ctxLogger ctx
    mgr   = ctxManager ctx

type M = Integrations '[I, I, Proxy, Proxy, Proxy, I]

fetcher :: M ([PMUser], Map FUM.Login FUM.User, [P.Employee])
fetcher = liftA3 (,,) users fum (P.personio P.PersonioEmployees)
  where
    fum = Map.fromList . map (\u -> (u ^. FUM.userName, u)) . toList <$> fumEmployeeList

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

defaultMain :: IO ()
defaultMain = futuriceServerMain makeCtx $ emptyServerConfig
    & serverName                .~ "PlanMill Sync"
    & serverDescription         .~ "Sync people from personio to planmill"
    & serverApp planmillSyncApi .~ server
    & serverColour              .~  (Proxy :: Proxy ('FutuAccent 'AF5 'AC1))
    & serverEnvPfx              .~ "PLANMILLSYNC"
    & serverOpts                .~ optionsFlag True [(True, "planmill-direct"), (False, "planmill-proxy")] "Access PlanMill directly"
  where
    makeCtx :: Bool -> Config -> Logger -> Manager -> Cache -> IO (Ctx, [Job])
    makeCtx planmillDirect cfg lgr mgr cache = do
        ws <-
            if planmillDirect
            then Just <$> workers lgr mgr (cfgPlanMillCfg cfg) ["worker1", "worker2", "worker3"]
            else pure Nothing
        let ctx = Ctx cfg lgr mgr cache ws
        pure (ctx, [])
