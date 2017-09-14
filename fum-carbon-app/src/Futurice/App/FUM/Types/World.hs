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
    worldEmployeeGroups,
    ) where

import Control.Lens     (contains)
import Futurice.IdMap   (IdMap)
import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.Types.Basic

import qualified Futurice.IdMap as IdMap

-- import qualified Personio as P

-- | World desribes the state of the db.
data World = World
    { _worldEmployees  :: !(IdMap Employee)
    , _worldCustomers  :: !(IdMap Customer)
    , _worldMailboxes  :: !(IdMap Mailbox)
    , _worldGroups     :: !(IdMap Group)
    , _worldSudoGroup  :: !(Maybe GroupName) -- change to Group!
    -- * lazy fields
    , _worldEmployeeGroups :: Map Login (IdMap Group)
    }

worldEmployees :: Lens' World (IdMap Employee)
worldEmployees f w = fmap
    (\x -> remakeWorld w { _worldEmployees = x})
    (f (_worldEmployees w))
{-# INLINE worldEmployees #-}

worldCustomers :: Lens' World (IdMap Customer)
worldCustomers f w = fmap
    (\x -> remakeWorld w { _worldCustomers = x})
    (f (_worldCustomers w))
{-# INLINE worldCustomers #-}

worldMailboxes :: Lens' World (IdMap Mailbox)
worldMailboxes f w = fmap
    (\x -> remakeWorld w { _worldMailboxes = x})

    (f (_worldMailboxes w))
{-# INLINE worldMailboxes #-}

worldGroups :: Lens' World (IdMap Group)
worldGroups f w = fmap
    (\x -> remakeWorld w { _worldGroups = x})
    (f (_worldGroups w))
{-# INLINE worldGroups #-}

worldSudoGroup :: Lens' World (Maybe GroupName)
worldSudoGroup f w = fmap
    (\x -> remakeWorld w { _worldSudoGroup = x})
    (f (_worldSudoGroup w))
{-# INLINE worldSudoGroup #-}

worldEmployeeGroups :: Getter World (Map Login (IdMap Group))
worldEmployeeGroups = getter _worldEmployeeGroups
{-# INLINE worldEmployeeGroups #-}

emptyWorld :: World
emptyWorld = makeWorld mempty mempty mempty mempty Nothing

makeWorld
    :: IdMap Employee
    -> IdMap Customer
    -> IdMap Mailbox
    -> IdMap Group
    -> Maybe GroupName
    -> World
makeWorld es cs ms gs sg = World es cs ms gs sg employeeGroups
  where
    employeeGroups = toMapOf (IdMap.ifolded . getter mk) es where
        mk e = IdMap.filter (\g -> g ^. groupEmployees . contains l) gs where
            l = e ^. employeeLogin

remakeWorld :: World -> World
remakeWorld w = makeWorld
    (_worldEmployees w)
    (_worldCustomers w)
    (_worldMailboxes w)
    (_worldGroups w)
    (_worldSudoGroup w)

nullWorld :: World -> Bool
nullWorld (World es cs ms gs sg _) =
    null es && null cs && null ms && null gs && null sg

-------------------------------------------------------------------------------
-- Validation
-------------------------------------------------------------------------------

-- | TODO: perform "GC"
validateWorld :: World -> World
validateWorld = id

-------------------------------------------------------------------------------
-- ToJSON / ToSchema
-------------------------------------------------------------------------------

-- TODO
