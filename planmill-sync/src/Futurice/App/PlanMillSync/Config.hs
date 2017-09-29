module Futurice.App.PlanMillSync.Config (
    Config (..),
    ) where

import Futurice.EnvConfig
import Futurice.Integrations
import Futurice.Prelude
import Prelude ()

import qualified PlanMill as PM

data Config = Config
    { cfgIntegrationsConfig :: !(IntegrationsConfig I Proxy Proxy Proxy I)
    , cfgPlanMillCfg        :: !PM.Cfg
    }

instance Configure Config where
    configure = Config
        <$> configure
        <*> configure
