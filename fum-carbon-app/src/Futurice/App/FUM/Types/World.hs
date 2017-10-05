{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.FUM.Types.World (
    World,
    emptyWorld,
    nullWorld,
    validateWorld,
    -- * Lenses
    worldEmployees,
    worldCustomers,
    worldMailboxes,
    worldGroups,
    worldSudoGroup,
    worldNextUID,
    worldNextGID,
    -- * Getters
    worldEmployeeGroups,
    worldEmails,
    ) where

import Control.Lens         (contains, toListOf)
import Control.Monad.Reader (asks)
import Data.Set.Lens        (setOf)
import Futurice.IdMap       (IdMap)
import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.Types.Basic
import Futurice.App.FUM.Types.UnixID

import qualified Data.Set       as Set
import qualified Futurice.IdMap as IdMap

-- import qualified Personio as P

-- | World desribes the state of the db.
data World = World
    { _worldEmployees  :: !(IdMap Employee)
    , _worldCustomers  :: !(IdMap Customer)
    , _worldMailboxes  :: !(IdMap Mailbox)
    , _worldGroups     :: !(IdMap Group)
    , _worldSudoGroup  :: !(Maybe GroupName) -- change to Group!
    , _worldNextUID    :: !UID
    , _worldNextGID    :: !GID
    -- lazy fields
    , _worldEmployeeGroups :: Map Login (IdMap Group)
    }

worldEmployees :: Lens' World (IdMap Employee)
worldEmployees f w = fmap
    (\x -> w { _worldEmployees = x})
    (f (_worldEmployees w))
{-# INLINE worldEmployees #-}

worldCustomers :: Lens' World (IdMap Customer)
worldCustomers f w = fmap
    (\x -> w { _worldCustomers = x})
    (f (_worldCustomers w))
{-# INLINE worldCustomers #-}

worldMailboxes :: Lens' World (IdMap Mailbox)
worldMailboxes f w = fmap
    (\x -> w { _worldMailboxes = x})
    (f (_worldMailboxes w))
{-# INLINE worldMailboxes #-}

worldGroups :: Lens' World (IdMap Group)
worldGroups f w = fmap
    (\x -> w { _worldGroups = x})
    (f (_worldGroups w))
{-# INLINE worldGroups #-}

worldSudoGroup :: Lens' World (Maybe GroupName)
worldSudoGroup f w = fmap
    (\x -> w { _worldSudoGroup = x})
    (f (_worldSudoGroup w))
{-# INLINE worldSudoGroup #-}

worldNextUID :: Lens' World UID
worldNextUID f w = fmap
    (\x -> w { _worldNextUID = x})
    (f (_worldNextUID w))
{-# INLINE worldNextUID #-}

worldNextGID :: Lens' World GID
worldNextGID f w = fmap
    (\x -> w { _worldNextGID = x})
    (f (_worldNextGID w))
{-# INLINE worldNextGID #-}

worldEmployeeGroups :: Getter World (Map Login (IdMap Group))
worldEmployeeGroups = getter _worldEmployeeGroups
{-# INLINE worldEmployeeGroups #-}

-- | TODO: change to Map Email Owner?
worldEmails :: Getter World (Set Email)
worldEmails = getter $ \w -> mconcat
    [ setOf (worldEmployees . folded . employeeEmail) w
    , setOf (worldEmployees . folded . employeeEmailAliases . folded) w
    , setOf (worldGroups . folded . groupEmailAliases . folded) w
    , setOf (worldMailboxes . folded . mailboxEmail) w
    , setOf (worldMailboxes . folded . mailboxEmailAliases . folded) w
    ]

-- | Empty, virgin world.
emptyWorld :: World
emptyWorld = World
    { _worldEmployees      = mempty
    , _worldCustomers      = mempty
    , _worldMailboxes      = mempty
    , _worldGroups         = mempty
    , _worldSudoGroup      = Nothing
    , _worldNextUID        = firstUnixID
    , _worldNextGID        = firstUnixID
    , _worldEmployeeGroups = mempty
    }

makeWorld :: World -> World
makeWorld World
    { _worldEmployees = es
    , _worldCustomers = cs
    , _worldMailboxes = ms
    , _worldGroups    = gs
    , _worldSudoGroup = sg
    , _worldNextUID   = uid
    , _worldNextGID   = gid
    } = World
    { _worldEmployees      = es
    , _worldCustomers      = cs
    , _worldMailboxes      = ms
    , _worldGroups         = gs
    , _worldSudoGroup      = sg
    , _worldNextUID        = uid
    , _worldNextGID        = gid
    , _worldEmployeeGroups = employeeGroups
    }
  where
    employeeGroups = toMapOf (IdMap.ifolded . getter mk) es where
        mk e = IdMap.filter (\g -> g ^. groupEmployees . contains l) gs where
            l = e ^. employeeLogin

nullWorld :: World -> Bool
nullWorld (World es cs ms gs sg uid gid _) =
    null es && null cs && null ms && null gs && null sg
    && uid == firstUnixID
    && gid == firstUnixID

-------------------------------------------------------------------------------
-- Validation
-------------------------------------------------------------------------------

validateWorld :: (MonadReader World m, MonadState World m, MonadError String m) => m ()
validateWorld = do
    -- UIDS should be unique
    uids <- asks $ toListOf (worldEmployees . folded . employeeUID)
    for_ (firstDuplicate uids) $ \dupUid ->
        throwError $ "Duplicate uid: "++ show dupUid

    -- todo check nextUID is bigger

    -- GIDs should be unique
    gids <- asks $ toListOf (worldGroups . folded . groupGID)
    for_ (firstDuplicate gids) $ \dupgid ->
        throwError $ "Duplicate gid: "++ show dupgid

    -- todo check nextGID is bigger

    id %= makeWorld

firstDuplicate :: (Foldable f, Ord a) => f a -> Maybe a
firstDuplicate = go Set.empty . toList where
    go !_ []                = Nothing
    go !xs (y:ys)
        | y `Set.member` xs = Just y
        | otherwise         = go (Set.insert y xs) ys

-------------------------------------------------------------------------------
-- ToJSON / ToSchema
-------------------------------------------------------------------------------

-- TODO
