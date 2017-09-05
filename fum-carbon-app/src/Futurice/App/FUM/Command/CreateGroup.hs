{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.FUM.Command.CreateGroup (CreateGroup (..)) where

import Data.Maybe        (isJust)
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.Command.Definition
import Futurice.App.FUM.Pages.Href
import Futurice.App.FUM.Types

import qualified Data.Set as Set

data CreateGroup (phase :: Phase) = CreateGroup
    { cgName :: !GroupName
    , cgType :: !GroupType
    }
  deriving (Show, Typeable, Generic)

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

    internalizeCommand _now _login rights cmd = do
        requireRights RightsNormal rights
        validate cmd
        pure (coerce cmd)

    applyCommand _now login cmd = do
        validate cmd

        let name = cgName cmd
        worldGroups . at name ?= Group
            { _groupName         = name
            , _groupType         = cgType cmd
            , _groupDescription  = "" -- TODO
            , _groupEmailAliases = []
            , _groupEditor       = mempty
            , _groupEmployees    = Set.singleton login  -- creator is in the group
            , _groupCustomers    = mempty
            }

        pure $ LomakeResponseRedirect $ viewGroupHrefText name

validate :: (MonadReader World m, MonadError String m) => CreateGroup phase -> m ()
validate cmd = do
    let name = cgName cmd
    whenM (fmap isJust $ preview $ worldGroups . ix name) $
        throwError $ "Group with name " ++ show (groupNameToText name) ++ " already exists"