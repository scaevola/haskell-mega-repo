{-# LANGUAGE FlexibleContexts #-}
-- | Functions related to checking access rights.
module Futurice.App.FUM.ACL where

import Control.Lens     (Getting, contains, has, hasn't)
import Data.Monoid      (All, Any)
import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.Types

unlessExists
    :: MonadReader World m
    => Getting All World a
    -> m ()
    -> m ()
unlessExists l action = do
    w <- view id
    when (hasn't l w) action

whenExists
    :: MonadReader World m
    => Getting Any World a
    -> m ()
    -> m ()
whenExists l action = do
    w <- view id
    when (has l w) action

-- | Can login edit group?
--
-- * if 'isSudoer'
--
-- * if there aren't editor groups => if member of the group
--
-- * if there are editor groups => member of any editor group
--
canEditGroup
    :: MonadReader World m
    => Login -- ^ editor
    -> GroupName
    -> m Bool
canEditGroup l _ = isSudoer l
-- TODO: implement rest

-- | Can login edit other login?
--
-- * if `isSudoer`
--
-- * if same
canEditEmployee
    :: MonadReader World m
    => Login -- ^ editor
    -> Login
    -> m Bool
canEditEmployee l l' | l == l' = pure True
canEditEmployee l _ = isSudoer l

isSudoer :: MonadReader World m => Login -> m Bool
isSudoer login = fmap (fromMaybe False) $ runMaybeT $ do
    gn <- MaybeT $ view worldSudoGroup
    MaybeT $ preview (worldGroups . ix gn . groupEmployees . contains login)
