{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.App.FUM.Command.AddEmailToEmployee (AddEmailToEmployee) where

import Control.Lens      (contains, (.=))
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.ACL
import Futurice.App.FUM.Command.Definition
import Futurice.App.FUM.Pages.Href
import Futurice.App.FUM.Types

data AddEmailToEmployee (phase :: Phase) = AddEmailToEmployee
    { aeeLogin :: !Login
    , aeeEmail :: !Email
    }
  deriving (Show, Typeable, Generic)

deriveGeneric ''AddEmailToEmployee

instance phase ~ 'Input => HasLomake (AddEmailToEmployee phase) where
    lomake _ =
        hiddenField "Login" :*
        textFieldWithRegexp "Email address" emailKleene :*
        Nil

instance phase ~ 'Internal => ToJSON (AddEmailToEmployee phase) where
    toJSON = sopToJSON
    toEncoding = sopToEncoding

instance phase ~ 'Internal => FromJSON (AddEmailToEmployee phase) where
    parseJSON = sopParseJSON

instance Command AddEmailToEmployee where
    type CommandTag AddEmailToEmployee = "add-email-to-employee"

    internalizeCommand _now login rights cmd = do
        requireRights RightsNormal rights
        validate login cmd
        pure (coerce cmd)

    applyCommand _now login' cmd = do
        validate login' cmd
        let login = aeeLogin cmd

        worldEmployees . ix login . employeeEmailAliases . contains (aeeEmail cmd) .= True

        pure $ LomakeResponseRedirect $ viewEmployeeHrefText login

validate
    :: (MonadReader World m, MonadError String m)
    => Login -> AddEmailToEmployee phase -> m ()
validate login' cmd = do
    let login = aeeLogin cmd

    -- user exists
    unlessExists (worldEmployees . ix login) $
        throwError $ "Employee doesn't exist " ++ show (loginToText login)

    -- check rights
    unlessM (canEditEmployee login' login) $
        throwError $ "You cannot edit user " ++ show (loginToText login)

    -- Check that emails is unique
    emails <- view worldEmails
    when (emails ^. contains (aeeEmail cmd)) $
        throwError $ "Email already in use"
