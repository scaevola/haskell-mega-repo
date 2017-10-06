{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
module Futurice.App.FUM.Command (
    Commands,
    SomeCommand,
    someCommand,
    withSomeCommand,
    CT,
    ICT,
    withCT,
    decodeSomeCommand,
    module Futurice.App.FUM.Command.AddEmailToEmployee,
    module Futurice.App.FUM.Command.AddEmployeeToGroup,
    module Futurice.App.FUM.Command.Bootstrap,
    module Futurice.App.FUM.Command.Definition,
    module Futurice.App.FUM.Command.CreateEmployee,
    module Futurice.App.FUM.Command.CreateGroup,
    module Futurice.App.FUM.Command.RemoveEmailFromEmployee,
    module Futurice.App.FUM.Command.RemoveEmployeeFromGroup,
    ) where

import Control.Lens       (review)
import Data.Aeson.Types   (parseEither, parseJSON)
import Data.Constraint    (Dict (..))
import Data.Type.Equality
import Futurice.Has       (In, inj)
import Futurice.Prelude
import Futurice.TypeTag
import Generics.SOP       (hcmap, hcollapse)
import Prelude ()

import Futurice.App.FUM.Command.AddEmailToEmployee
import Futurice.App.FUM.Command.AddEmployeeToGroup
import Futurice.App.FUM.Command.Bootstrap
import Futurice.App.FUM.Command.CreateEmployee
import Futurice.App.FUM.Command.CreateGroup
import Futurice.App.FUM.Command.Definition
import Futurice.App.FUM.Command.RemoveEmailFromEmployee
import Futurice.App.FUM.Command.RemoveEmployeeFromGroup

import qualified Data.Map as Map

-- | List of commands
type Commands = '[ Bootstrap
    , AddEmailToEmployee
    , AddEmployeeToGroup
    , CreateEmployee
    , CreateGroup
    , RemoveEmailFromEmployee
    , RemoveEmployeeFromGroup
    ]

-- | Existential command, union of all commands.
data SomeCommand where
    SomeCommand :: CT cmd -> cmd 'Internal -> SomeCommand

type ICT cmd = In cmd Commands

-- | 'SomeCommand' introduction.
someCommand :: (Command cmd, ICT cmd) => cmd 'Internal -> SomeCommand
someCommand = SomeCommand (TT (review inj Refl))

-- | 'SomeCommand' elimination.
withSomeCommand
    :: SomeCommand
    -> (forall cmd. Command cmd => CT cmd -> cmd 'Internal -> r)
    -> r
withSomeCommand (SomeCommand tag cmd) f = withCT tag (f tag cmd)

type CT = TT Commands

withCT :: forall cmd r. CT cmd -> (Command cmd => r) -> r
withCT ct r = case typeTagDict (Proxy :: Proxy Command) ct of
    Dict -> r

decodeSomeCommand :: Text -> Value -> Either String SomeCommand
decodeSomeCommand name payload = case Map.lookup name typeTagsByName of
    Nothing ->
        Left $ "Unknown  command: " ++ show name ++ " = " ++ show payload
    Just (SomeTT tag) -> withCT tag $
        SomeCommand tag <$> parseEither parseJSON payload

typeTagsByName :: Map Text (SomeTT Commands)
typeTagsByName = Map.fromList $ hcollapse $ hcmap (Proxy :: Proxy Command) f (typeTags :: NP CT Commands)
  where
    f :: forall cmd. Command cmd => TT Commands cmd -> K (Text, SomeTT Commands) cmd
    f tag = K (commandTag tag, SomeTT tag)
