module Futurice.App.Smileys.Config (
    Config(..),
    ) where

import Futurice.Prelude
import Prelude ()

import Database.PostgreSQL.Simple (ConnectInfo)
import FUM.Types.Login            (Login)
import Futurice.EnvConfig

data Config = Config
    { cfgMockUser          :: !(Maybe Login)
    , cfgPostgresConnInfo  :: !ConnectInfo
    }
    deriving (Show)

instance Configure Config where
    configure = Config
        <$> optionalAlt (envVar "MOCKUSER")
        <*> envConnectInfo

