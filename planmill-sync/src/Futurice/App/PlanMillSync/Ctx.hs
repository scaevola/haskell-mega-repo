module Futurice.App.PlanMillSync.Ctx (Ctx (..)) where

import Futurice.Prelude
import Prelude ()

import Futurice.App.PlanMillSync.Config

data Ctx = Ctx
    { ctxConfig  :: !Config
    , ctxLogger  :: !Logger
    , ctxManager :: !Manager
    }
