{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
module Futurice.App.FUM.Command (
    SomeCommand,
    someCommand,
    withSomeCommand,
    CT (..),
    ICT (..),
    withCT,
    decodeSomeCommand,
    module Futurice.App.FUM.Command.AddEmailToEmployee,
    module Futurice.App.FUM.Command.AddEmployeeToGroup,
    module Futurice.App.FUM.Command.Bootstrap,
    module Futurice.App.FUM.Command.Definition,
    module Futurice.App.FUM.Command.CreateEmployee,
    module Futurice.App.FUM.Command.CreateGroup,
    ) where

import Futurice.Prelude
import Prelude ()
import Data.Aeson.Types (parseEither, parseJSON)
import Data.Functor.Alt ((<!>))

import Futurice.App.FUM.Command.AddEmailToEmployee
import Futurice.App.FUM.Command.AddEmployeeToGroup
import Futurice.App.FUM.Command.Bootstrap
import Futurice.App.FUM.Command.CreateEmployee
import Futurice.App.FUM.Command.CreateGroup
import Futurice.App.FUM.Command.Definition

-- | Existential command, union of all commands.
data SomeCommand where
    SomeCommand :: CT cmd -> cmd 'Internal -> SomeCommand

-- | 'SomeCommand' introduction.
someCommand :: (Command cmd, ICT cmd) => cmd 'Internal -> SomeCommand
someCommand = SomeCommand icommandTag

-- | 'SomeCommand' elimination.
withSomeCommand
    :: SomeCommand
    -> (forall cmd. Command cmd => CT cmd -> cmd 'Internal -> r)
    -> r
withSomeCommand (SomeCommand tag cmd) f = withCT tag (f tag cmd)

-- | GADT representing different commands.
data CT cmd where
    CTAddEmailToEmployee :: CT AddEmailToEmployee
    CTAddEmployeeToGroup :: CT AddEmployeeToGroup
    CTBootstrap          :: CT Bootstrap
    CTCreateEmployee     :: CT CreateEmployee
    CTCreateGroup        :: CT CreateGroup

deriving instance Show (CT cmd)

-- | Implicit 'CT'.
class    ICT cmd                where icommandTag :: CT cmd
instance ICT AddEmailToEmployee where icommandTag = CTAddEmailToEmployee
instance ICT AddEmployeeToGroup where icommandTag = CTAddEmployeeToGroup
instance ICT Bootstrap          where icommandTag = CTBootstrap
instance ICT CreateEmployee     where icommandTag = CTCreateEmployee
instance ICT CreateGroup        where icommandTag = CTCreateGroup

withCT :: CT cmd -> (Command cmd => r) -> r
withCT CTAddEmailToEmployee f = f
withCT CTAddEmployeeToGroup f = f
withCT CTBootstrap f          = f
withCT CTCreateEmployee f     = f
withCT CTCreateGroup f        = f

decodeSomeCommand :: Text -> Value -> Either String SomeCommand
decodeSomeCommand name payload =
    ct CTAddEmailToEmployee <!>
    ct CTAddEmployeeToGroup <!>
    ct CTBootstrap <!>
    ct CTCreateEmployee <!>
    ct CTCreateGroup <!>
    Left ("Unknown or corrupt command: " ++ show name ++ " = " ++ show payload)
  where
    ct :: CT cmd -> Either String SomeCommand
    ct tag = withCT tag $
        if commandTag tag == name
        then SomeCommand tag <$> parseEither parseJSON payload
        else Left $ "Tag doesn't match: " ++ show name
