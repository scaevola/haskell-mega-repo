module Futurice.App.Smileys.Ctx (
    Ctx(..),
    ) where

import FUM.Types.Login   (Login)
import Futurice.Cache    (Cache)
import Futurice.Postgres
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

instance HasPostgresPool Ctx where
    postgresPool = ctxPostgresPool 
