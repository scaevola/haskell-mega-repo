{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Futurice.App.FUM (defaultMain) where

import Control.Concurrent.STM         (atomically, writeTVar)
import Futurice.Lomake
import Futurice.Periocron
import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Servant

import Futurice.App.FUM.API
import Futurice.App.FUM.Auth
import Futurice.App.FUM.Command
import Futurice.App.FUM.Config
import Futurice.App.FUM.Ctx
import Futurice.App.FUM.Machine
import Futurice.App.FUM.Markup
import Futurice.App.FUM.Pages.Server
import Futurice.App.FUM.Report.Validation
import Futurice.App.FUM.Transactor
import Futurice.App.FUM.Types

import qualified Futurice.IdMap as IdMap
import qualified Personio

-------------------------------------------------------------------------------
-- Server
-------------------------------------------------------------------------------

cmdServer
    :: forall cmd. (Command cmd, ICT cmd)
    => Ctx -> Server (CommandEndpoint cmd)
cmdServer ctx mlogin (LomakeRequest cmdInput) = runLogT "command" (ctxLogger ctx) $
    withAuthUser' (error "lomake error") ctx mlogin $ \(AuthUser login rights) world _ -> do
        now <- currentTime
        cmdInternal' <- hoist liftIO $ runExceptT $
            runReaderT (internalizeCommand now login rights cmdInput) world
        case cmdInternal' of
            Left err -> pure (LomakeResponseError err)
            Right cmdInternal -> do
                res <- hoist liftIO $ transact ctx now login (someCommand cmdInternal)
                case res of
                    Right res' -> pure res'
                    Left err   -> pure (LomakeResponseError err)

commandServer :: Ctx -> Server FumCarbonCommandApi
commandServer ctx = cmdServer ctx
    :<|> cmdServer ctx
    :<|> cmdServer ctx
    :<|> cmdServer ctx

server :: Ctx -> Server FumCarbonApi
server ctx = pagesServer ctx
    :<|> validationReportImpl ctx
    :<|> commandServer ctx
    :<|> machineServer ctx

-------------------------------------------------------------------------------
-- Reports
-------------------------------------------------------------------------------

validationReportImpl :: Ctx -> Handler (HtmlPage "validation-report")
validationReportImpl = liftIO . validationReport

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

defaultMain :: IO ()
defaultMain = futuriceServerMain makeCtx $ emptyServerConfig
    & serverName             .~ "FUM Carbon"
    & serverDescription      .~ "FUM faster than ever"
    & serverColour           .~ (Proxy :: Proxy ('FutuAccent 'AF4 'AC3))
    & serverApp fumCarbonApi .~ server
    & serverEnvPfx           .~ "FUMAPP"

makeCtx :: Config -> Logger -> DynMapCache -> IO (Ctx, [Job])
makeCtx Config {..} lgr _cache = do
    mgr <- newManager tlsManagerSettings

    -- employees
    let fetchEmployees = Personio.evalPersonioReqIO mgr lgr cfgPersonioCfg Personio.PersonioAll
    (employees, validations) <- fetchEmployees

    -- context
    ctx <- newCtx
        lgr
        cfgMockUser
        cfgPostgresConnInfo
        (IdMap.fromFoldable employees)
        validations

    -- jobs
    let employeesJob = mkJob "Update personio data" (updateJob ctx fetchEmployees) $ tail $ every 300

    pure (ctx, [ employeesJob ])
  where
    updateJob :: Ctx -> IO ([Personio.Employee], [Personio.EmployeeValidation]) -> IO ()
    updateJob ctx fetchEmployees = do
        (employees, validations) <- fetchEmployees
        atomically $ do
            writeTVar (ctxPersonio ctx) (IdMap.fromFoldable employees)
            writeTVar (ctxPersonioValidations ctx) validations
