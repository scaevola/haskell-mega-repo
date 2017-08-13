{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}
module Futurice.App.FUM.Command (
    SomeCommand,
    someCommand,
    withSomeCommand,
    module Futurice.App.FUM.Command.Definition,
    module Futurice.App.FUM.Command.CreateEmployee,
    ) where

import Prelude ()

import Futurice.App.FUM.Command.CreateEmployee
import Futurice.App.FUM.Command.Definition

-- | Existential command, union of all commands.
data SomeCommand where
    SomeCommand :: Command cmd => cmd 'Internal -> SomeCommand

-- | 'SomeCommand' introduction.
someCommand :: Command cmd => cmd 'Internal -> SomeCommand
someCommand = SomeCommand

-- | 'SomeCommand' elimination.
withSomeCommand
    :: SomeCommand
    -> (forall cmd. Command cmd => cmd 'Internal -> r)
    -> r
withSomeCommand (SomeCommand cmd) f = f cmd
