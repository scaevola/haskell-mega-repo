module Futurice.App.ProxyMgmt.Ctx (
    Ctx (..),
    ) where

import Dashdo.Servant                (DashdoAPI)
import Futurice.App.ProxyMgmt.Config
import Futurice.Postgres
import Futurice.Prelude
import Futurice.Servant              (Cache)
import Prelude ()
import Servant                       (Server)

data Ctx f = Ctx
    { ctxPostgresPool :: !(Pool Connection)
    , ctxConfig       :: !Config
    , ctxLogger       :: !Logger
    , ctxCache        :: !Cache
    , ctxManager      :: !Manager
    , ctxDashdoServer :: !(f (Server DashdoAPI))
    }
