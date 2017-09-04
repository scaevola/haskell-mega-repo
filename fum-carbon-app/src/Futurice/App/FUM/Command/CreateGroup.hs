{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.FUM.Command.CreateGroup where

-- import Control.Lens      (preuse)
-- import Data.Maybe        (isJust)
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.Command.Definition
import Futurice.App.FUM.Pages.Href
import Futurice.App.FUM.Types

data CreateGroup (phase :: Phase) = CreateGroup
    { cgName :: !GroupName
    , cgType :: !GroupType
    }
  deriving (Show, Typeable, Generic)

makeLenses ''CreateGroup
deriveGeneric ''CreateGroup

instance phase ~ 'Input => HasLomake (CreateGroup phase) where
    lomake _ =
        textField "name" :*
        enumField "type" :*
        -- todo description
        Nil

instance phase ~ 'Internal => ToJSON (CreateGroup phase) where
    toJSON = sopToJSON
    toEncoding = sopToEncoding

instance phase ~ 'Internal => FromJSON (CreateGroup phase) where
    parseJSON = sopParseJSON

instance Command CreateGroup where
    type CommandTag CreateGroup = "create-group"

    -- TODO: check that group name is unique
    internalizeCommand _now _login cmd = pure $ coerce cmd

    -- TODO:
    applyCommand _now _login cmd = do
        let name = cgName cmd 
        -- TODO: check that group name is unique

        pure $ LomakeResponseRedirect $ viewGroupHrefText name
