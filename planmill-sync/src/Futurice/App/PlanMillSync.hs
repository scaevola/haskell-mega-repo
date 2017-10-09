{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.PlanMillSync (defaultMain) where

import Futurice.Integrations
import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Futurice.Lucid.Foundation (page_, h1_, fullRow_)
import Servant

import Futurice.FUM.MachineAPI (FUM6 (..), fum6)

import Futurice.App.PlanMillSync.API
import Futurice.App.PlanMillSync.Config
import Futurice.App.PlanMillSync.Ctx
import Futurice.App.PlanMillSync.IndexPage
import Futurice.App.PlanMillSync.Types

import qualified Data.Set        as Set
import qualified FUM.Types.Login as FUM
import qualified Personio        as P

server :: Ctx -> Server PlanMillSyncAPI
server ctx = indexPageAction ctx
    -- TODO: actions to add & remove

indexPageAction
    :: Ctx
    -> Maybe FUM.Login
    -> Handler (HtmlPage "index")
indexPageAction ctx mfu = do
    now <- currentTime

    -- Access control
    fus <- liftIO $ runIntegrations mgr lgr now (cfgIntegrationsConfig2 cfg) $
        fum6 $ FUMGroupEmployees (cfgAccessGroup cfg)
    liftIO $ print fus

    case mfu <|> cfgMockUser cfg of
        Just fu | Set.member fu fus -> do
            -- Data fetch
            (pm, p) <- liftIO $
                runIntegrations mgr lgr now (cfgIntegrationsConfig cfg) fetcher

            -- Render
            pure $ indexPage now pm p

        _ -> pure page404 -- TODO: log unauhtorised access?
  where
    cfg = ctxConfig ctx
    lgr = ctxLogger ctx
    mgr = ctxManager ctx

page404 :: HtmlPage a
page404 = page_ "PlanMill Sync - Unauthorised" $ 
    fullRow_ $ do
        h1_ "Unauthorised"
        "Ask IT Team for access rights"

type M = Integrations I Proxy Proxy Proxy Proxy I

fetcher :: M ([PMUser], [P.Employee])
fetcher = liftA2 (,) users (P.personio P.PersonioEmployees)

defaultMain :: IO ()
defaultMain = futuriceServerMain makeCtx $ emptyServerConfig
    & serverName              .~ "PlanMill Sync"
    & serverDescription       .~ "Sync people from personio to planmill"
    & serverApp githubSyncApi .~ server
    & serverColour            .~  (Proxy :: Proxy ('FutuAccent 'AF5 'AC1))
    & serverEnvPfx            .~ "PLANMILLSYNC"
  where
    makeCtx :: Config -> Logger -> Cache -> IO (Ctx, [Job])
    makeCtx cfg lgr _cache = do
        mgr <- newManager tlsManagerSettings
        let ctx = Ctx cfg lgr mgr
        pure (ctx, [])
