module Futurice.App.Smileys.Ctx (
    Ctx(..),
    ) where

import Data.Pool                  (Pool)
import Database.PostgreSQL.Simple (Connection)
import FUM.Types.Login            (Login)
import Futurice.Cache             (Cache)
import Futurice.Prelude
import Prelude ()

-------------------------------------------------------------------------------
-- Context
-------------------------------------------------------------------------------

data Ctx = Ctx
    { ctxPostgresPool :: !(Pool Connection)
    , ctxCache        :: !Cache
    , ctxLogger       :: !Logger
    , ctxMockUser     :: !(Maybe Login)
    }
