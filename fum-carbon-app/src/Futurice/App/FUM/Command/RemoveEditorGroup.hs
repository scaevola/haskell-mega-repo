{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.App.FUM.Command.RemoveEditorGroup (RemoveEditorGroup) where

import Control.Lens      (contains, (.=))
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.ACL
import Futurice.App.FUM.Command.Definition
import Futurice.App.FUM.Pages.Href
import Futurice.App.FUM.Types

data RemoveEditorGroup (phase :: Phase) = RemoveEditorGroup
    { regName   :: !GroupName
    , regEditor :: !GroupName
    }
  deriving (Show, Typeable, Generic)

deriveGeneric ''RemoveEditorGroup

instance phase ~ 'Input => HasLomake (RemoveEditorGroup phase) where
    lomake _ =
        dynEnumField "Group" :*
        dynEnumField "Editor group" :*
        Nil

instance phase ~ 'Internal => ToJSON (RemoveEditorGroup phase) where
    toJSON = sopToJSON
    toEncoding = sopToEncoding

instance phase ~ 'Internal => FromJSON (RemoveEditorGroup phase) where
    parseJSON = sopParseJSON

instance Command RemoveEditorGroup where
    type CommandTag RemoveEditorGroup = "remove-editor-group"

    internalizeCommand _now login rights cmd = do
        requireRights RightsIT rights
        validate login cmd
        pure (coerce cmd)

    applyCommand _now login cmd = do
        validate login cmd

        let name = regName cmd
        let editor = regEditor cmd
        worldGroups . ix name . groupEditor . contains editor .= False

        pure $ LomakeResponseRedirect $ viewGroupHrefText name

validate
    :: (MonadReader World m, MonadError String m)
    => Login -> RemoveEditorGroup phase -> m ()
validate _login cmd = do
    let name = regName cmd
    let editor = regEditor cmd

    unlessExists (worldGroups . ix name) $
        throwError $ "Group doesn't exist " ++ show (groupNameToText name)

    unlessExists (worldGroups . ix editor) $
        throwError $ "Group doesn't exist " ++ show (groupNameToText editor)

    unlessExists (worldGroups . ix name . groupEditor . ix editor) $
        throwError $ show editor ++ " is not an editor of " ++ show name
