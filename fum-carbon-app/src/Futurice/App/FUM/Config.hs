module Futurice.App.FUM.Config (
    Config(..),
    ) where

import Database.PostgreSQL.Simple (ConnectInfo)
import FUM.Types.Login            (Login)
import Futurice.EnvConfig
import Futurice.Prelude
import Prelude ()

import qualified Personio

data Config = Config
    { cfgPostgresConnInfo :: !ConnectInfo
    , cfgPersonioCfg      :: !(Personio.Cfg)
    , cfgMockUser         :: !(Maybe Login)
    }
    deriving (Show)

instance Configure Config where
    configure = Config
        <$> envConnectInfo
        <*> configure
        <*> optionalAlt (envVar "MOCKUSER")
