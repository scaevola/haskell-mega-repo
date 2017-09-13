{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Implementation of the machine API.
module Futurice.App.FUM.Machine (machineServer) where

import Control.Concurrent.STM (readTVarIO)
import Futurice.Prelude
import Prelude ()
import Servant

import Futurice.App.FUM.API
import Futurice.App.FUM.Ctx

import qualified Personio

machineServer :: Ctx -> Server FumCarbonMachineApi
machineServer ctx = personioRequest ctx
    :<|> rawEmployees ctx
    :<|> rawValidations ctx

personioRequest :: Ctx -> Personio.SomePersonioReq -> Handler Personio.SomePersonioRes
personioRequest ctx (Personio.SomePersonioReq res) = case res of
    Personio.PersonioEmployees   -> Personio.SomePersonioRes res <$> rawEmployees ctx
    Personio.PersonioValidations -> Personio.SomePersonioRes res <$> rawValidations ctx
    Personio.PersonioAll         -> do
        es <- rawEmployees ctx
        vs <- rawValidations ctx
        pure (Personio.SomePersonioRes res (es, vs))

rawEmployees :: Ctx -> Handler [Personio.Employee]
rawEmployees ctx = do
    es <- liftIO $ readTVarIO $ ctxPersonio ctx
    -- no filtering, all employees
    pure $ toList es

rawValidations :: Ctx -> Handler [Personio.EmployeeValidation]
rawValidations ctx = liftIO $ readTVarIO $ ctxPersonioValidations ctx
