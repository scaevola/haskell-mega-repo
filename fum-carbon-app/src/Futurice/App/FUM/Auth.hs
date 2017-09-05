{-# LANGUAGE MultiWayIf #-}
module Futurice.App.FUM.Auth where

import Control.Concurrent.STM (atomically, readTVar)
import Control.Lens           (contains, has)
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

        let rights = if
                | isSudoer fu world                  -> RightsIT
                | has (worldEmployees . ix fu) world -> RightsNormal
                | otherwise                          -> RightsOther

        f (AuthUser fu rights) world es

isSudoer :: Login -> World -> Bool
isSudoer login world = fromMaybe False $ do
    gn <- world ^. worldSudoGroup
    world ^? worldGroups . ix gn . groupEmployees . contains login
