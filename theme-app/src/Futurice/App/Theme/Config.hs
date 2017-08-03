module Futurice.App.Theme.Config (
    Config(..),
    ) where

import Prelude ()
import Futurice.Prelude
import Futurice.EnvConfig

data Config = Config deriving Show
instance Configure Config where
    configure = pure Config
