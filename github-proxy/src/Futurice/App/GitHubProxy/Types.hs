{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Futurice.App.GitHubProxy.Types (
    Ctx (..),
    ) where

import Futurice.Postgres
import Futurice.Prelude
import Futurice.Servant  (DynMapCache)
import GitHub.Auth       (Auth)
import Prelude ()

-------------------------------------------------------------------------------
-- Context
-------------------------------------------------------------------------------

data Ctx = Ctx
    { ctxCache        :: !DynMapCache
    , ctxGitHubAuth   :: !Auth
    , ctxPostgresPool :: !(Pool Connection)  -- TODO: write a lib to handle these
    , ctxLogger       :: !Logger
    }

instance HasPostgresPool Ctx where
    postgresPool = ctxPostgresPool
