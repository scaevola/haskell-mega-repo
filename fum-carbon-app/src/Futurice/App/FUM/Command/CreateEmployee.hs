{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.FUM.Command.CreateEmployee where

import Control.Lens      (has, use)
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.Command.Definition
import Futurice.App.FUM.Pages.Href
import Futurice.App.FUM.Types

import qualified Personio

data CreateEmployee (phase :: Phase) = CreateEmployee
    { cePersonioId :: !Personio.EmployeeId
    , ceLogin      :: !Login
    , ceStatus     :: !Status
    }
  deriving (Show, Typeable, Generic)

makeLenses ''CreateEmployee
deriveGeneric ''CreateEmployee

instance phase ~ 'Input => HasLomake (CreateEmployee phase) where
    lomake _ =
        hiddenField "personioId" :*
        hiddenField "login" :*
        enumField "status" :*
        Nil

instance phase ~ 'Internal => ToJSON (CreateEmployee phase) where
    toJSON = sopToJSON
    toEncoding = sopToEncoding

instance phase ~ 'Internal => FromJSON (CreateEmployee phase) where
    parseJSON = sopParseJSON

instance Command CreateEmployee where
    type CommandTag CreateEmployee = "create-employee"

    -- TODO: check that personioId and loginId aren't yet used!
    internalizeCommand _now cmd = pure $ coerce cmd

    -- TODO:
    applyCommand now cmd = do
        let login = ceLogin cmd
        world <- use id
        when (has (worldEmployees . ix login) world) $
            throwError $ "Employee with login " ++ show (loginToText login) ++ " already exists"

        worldEmployees . at login ?= Employee
            { _employeeLogin        = login
            , _employeePersonioId   = cePersonioId cmd
            , _employeeStatus       = ceStatus cmd
            , _employeeEmailAliases = []
            , _employeeSshKeys      = []
            , _employeePicture      = Nothing
            , _employeePasswordExp  = now  -- TODO
            }

        pure $ LomakeResponseRedirect $ viewEmployeeHrefText login
