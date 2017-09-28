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
import Servant

import Futurice.App.PlanMillSync.API
import Futurice.App.PlanMillSync.Config
import Futurice.App.PlanMillSync.Ctx
import Futurice.App.PlanMillSync.IndexPage

import qualified FUM.Types.Login  as FUM
import qualified Personio         as P
import qualified PlanMill         as PM
import qualified PlanMill.Queries as PMQ

server :: Ctx -> Server PlanMillSyncAPI
server ctx = indexPageAction ctx
    -- TODO: actions to add & remove

indexPageAction
    :: Ctx
    -> Maybe FUM.Login
    -> Handler (HtmlPage "index")
indexPageAction ctx _mfu = do
    -- TODO: restrict access to IT only
    now <- currentTime
    (pm, p) <- liftIO $
        runIntegrations mgr lgr now (cfgIntegrationsConfig cfg) fetcher
    pure $ indexPage now pm p
  where
    cfg = ctxConfig ctx
    lgr = ctxLogger ctx
    mgr = ctxManager ctx

type M = Integrations I Proxy Proxy Proxy I

fetcher :: M ([PM.User], [P.Employee])
fetcher = liftA2 (,) pms personios
  where
    pms = toList <$> planmillUsers 
    personios = P.personio P.PersonioEmployees

planmillUsers :: M (Vector PM.User)
planmillUsers =  do
    us <- PMQ.users
    traverse (PMQ.user . view PM.identifier) us

defaultMain :: IO ()
defaultMain = futuriceServerMain makeCtx $ emptyServerConfig
    & serverName              .~ "PlanMill Sync"
    & serverDescription       .~ "Sync people from personio to planmill"
    & serverApp githubSyncApi .~ server
    & serverColour            .~  (Proxy :: Proxy ('FutuAccent 'AF3 'AC1))
    & serverEnvPfx            .~ "PLANMILLSYNC"
  where
    makeCtx :: Config -> Logger -> Cache -> IO (Ctx, [Job])
    makeCtx cfg lgr _cache = do
        mgr <- newManager tlsManagerSettings
        let ctx = Ctx cfg lgr mgr
        pure (ctx, [])
