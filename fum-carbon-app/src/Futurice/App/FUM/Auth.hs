module Futurice.App.FUM.Auth where

import Control.Concurrent.STM (atomically, readTVar)
import Futurice.Prelude
import Prelude ()
import Servant                (Handler)

import Futurice.App.FUM.Ctx
import Futurice.App.FUM.Types

import qualified Futurice.IdMap as IdMap
import qualified Personio

-- | Wrap endpoint requiring authentication.
withAuthUser'
    :: a  -- ^ Response to unauthenticated users
    -> Ctx
    -> Maybe Login
    -> (AuthUser -> World -> IdMap.IdMap Personio.Employee -> LogT Handler a)
    -> LogT Handler a
withAuthUser' def ctx mfu f = case mfu <|> ctxMockUser ctx of
    Nothing -> pure def
    Just fu -> do
        (world, es) <- liftIO $ atomically $ (,)
             <$> readTVar (ctxWorld ctx)
             <*> readTVar (ctxPersonio ctx)
        -- TODO: check!
        f (fu, RightsIT) world es
