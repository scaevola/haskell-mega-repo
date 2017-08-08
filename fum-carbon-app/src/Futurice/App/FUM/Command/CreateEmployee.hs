{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.FUM.Command.CreateEmployee where

import Futurice.Generics
import Futurice.Lomake
import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.Command.Definition
import Futurice.App.FUM.Types

import qualified Personio

data CreateEmployee (phase :: Phase) = CreateEmployee
    { _cePersonioId :: !Personio.EmployeeId
    , _ceLogin      :: !Login
    , _ceStatus     :: !Status
    }
  deriving (Show, Typeable, Generic)

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
    commandTag _ = "create-employee"

    internalizeCommand _world cmd = pure $ Right $ coerce cmd

    -- TODO:
    applyCommand _cmd world = Right (LomakeResponseNoop, world)
