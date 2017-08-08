{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.HoursApi (defaultMain) where

import Control.Concurrent.STM  (newTVarIO, readTVarIO)
import Futurice.Integrations
import Futurice.Periocron
import Futurice.Prelude
import Futurice.Servant
import Network.HTTP.Client     (managerConnCount)
import Prelude ()
import Servant

import Futurice.App.HoursApi.API
import Futurice.App.HoursApi.Config
import Futurice.App.HoursApi.Ctx
import Futurice.App.HoursApi.Logic
       (entryDeleteEndpoint, entryEditEndpoint, entryEndpoint, hoursEndpoint,
       projectEndpoint, userEndpoint)
import Futurice.App.HoursApi.Monad (Hours, runHours)

import qualified PlanMill.Worker     as PM
import qualified FUM

server :: Ctx -> Server FutuhoursAPI
server ctx = pure "This is futuhours api"
    :<|> (\mfum        -> authorisedUser ctx mfum projectEndpoint)
    :<|> (\mfum        -> authorisedUser ctx mfum userEndpoint)
    :<|> (\mfum a b    -> authorisedUser ctx mfum (hoursEndpoint a b))
    :<|> (\mfum eu     -> authorisedUser ctx mfum (entryEndpoint eu))
    :<|> (\mfum eid eu -> authorisedUser ctx mfum (entryEditEndpoint eid eu))
    :<|> (\mfum eid    -> authorisedUser ctx mfum (entryDeleteEndpoint eid))

authorisedUser
    :: Ctx
    -> Maybe FUM.Login
    -> Hours a
    -> Handler a
authorisedUser ctx mfum action =
    mcase (mfum <|> ctxMockUser ctx) (throwError err403) $ \fumUsername -> do
        pmData <- liftIO $ readTVarIO $ ctxFumPlanmillMap ctx
        (fumUser, pmUser) <- maybe (throwError err403) pure $ pmData ^. at fumUsername
        runHours ctx pmUser (fromMaybe "" $ fumUser ^. FUM.userThumbUrl . lazy) action

defaultMain :: IO ()
defaultMain = futuriceServerMain makeCtx $ emptyServerConfig
    & serverName            .~ "Futuhours API"
    & serverDescription     .~ "Here we mark hours"
    & serverApp futuhoursApi .~ server
    -- TODO: remove this before going live:
    & serverMiddleware      .~ liftFuturiceMiddleware logStdoutDev
    & serverColour          .~  (Proxy :: Proxy ('FutuAccent 'AF2 'AC2))
    & serverEnvPfx          .~ "FUTUHOURSAPI"
  where
    makeCtx :: Config -> Logger -> DynMapCache -> IO (Ctx, [Job])
    makeCtx config lgr cache = do
        mgr <- newManager tlsManagerSettings
            { managerConnCount = 100
            }

        let integrConfig = cfgIntegrationsCfg config
        let getFumPlanmillMap = do
                now <- currentTime
                runIntegrations mgr lgr now integrConfig fumPlanmillMap
        let job = mkJob "Update Planmill <- FUM map"  getFumPlanmillMap $ every 600

        fpm <- getFumPlanmillMap
        fpmTVar <- newTVarIO fpm

        let pmCfg = cfgPlanmillCfg config
        ws <- PM.workers lgr mgr pmCfg ["worker1", "worker2", "worker3"]

        pure $ flip (,) [job] Ctx
            { ctxFumPlanmillMap  = fpmTVar
            , ctxPlanmillCfg     = cfgPlanmillCfg config
            , ctxMockUser        = cfgMockUser config
            , ctxManager         = mgr
            , ctxLogger          = lgr
            , ctxCache           = cache
            , ctxIntegrationsCfg = integrConfig
            , ctxWorkers         = ws
            }
