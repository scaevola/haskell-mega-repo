{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Futurice.App.FUM.Command.CreateEmployee (CreateEmployee (..)) where

import Data.Maybe        (isJust)
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.Command.Definition
import Futurice.App.FUM.Pages.Href
import Futurice.App.FUM.Types

import qualified Personio

data CreateEmployee (phase :: Phase) = CreateEmployee
    { cePersonioId :: !Personio.EmployeeId
    , ceUID        :: !(Phased phase () UID)
    , ceLogin      :: !Login
    , ceStatus     :: !Status
    , ceName       :: !Text
    , ceEmail      :: !Email
    }
  deriving (Typeable, Generic)

deriveGeneric ''CreateEmployee

instance phase ~ 'Input => HasLomake (CreateEmployee phase) where
    lomake _ =
        hiddenField "personioId" :*
        unitField :*
        hiddenField "login" :*
        enumField "status" statusToText :*
        hiddenField "name" :*
        hiddenField "email" :*
        Nil

instance phase ~ 'Internal => ToJSON (CreateEmployee phase) where
    toJSON = sopToJSON
    toEncoding = sopToEncoding

instance phase ~ 'Internal => FromJSON (CreateEmployee phase) where
    parseJSON = sopParseJSON

instance Command CreateEmployee where
    type CommandTag CreateEmployee = "create-employee"

    internalizeCommand _now _login rights cmd = do
        requireRights RightsIT rights
        validate cmd
        uid <- view worldNextUID
        pure cmd { ceUID = uid }

    applyCommand now _login cmd = do
        validate cmd

        let login = ceLogin cmd
        worldEmployees . at login ?= Employee
            { _employeeLogin        = login
            , _employeeUID          = ceUID cmd
            , _employeePersonioId   = cePersonioId cmd
            , _employeeStatus       = ceStatus cmd
            , _employeeName         = ceName cmd
            , _employeeEmail        = ceEmail cmd
            , _employeeEmailAliases = mempty
            , _employeeSshKeys      = []
            , _employeePicture      = Nothing
            , _employeePasswordExp  = now  -- TODO
            }

        -- make next UID one greater that any existing or current "next" UID.
        worldNextUID %= nextUnixID

        -- Redirect to the employee page
        pure $ LomakeResponseRedirect $ viewEmployeeHrefText login

validate :: (MonadReader World m, MonadError String m) => CreateEmployee phase -> m ()
validate  cmd = do
   let login = ceLogin cmd

   whenM (fmap isJust $ preview $ worldEmployees . ix login) $
       throwError $ "Employee with login " ++ show (loginToText login) ++ " already exists"
