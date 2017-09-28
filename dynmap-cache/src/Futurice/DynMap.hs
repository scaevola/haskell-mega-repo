{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2015-2017 Futurice Oy
-- License     :  BSD3
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- Concurrent dictionary with dynamic keys.
module Futurice.DynMap (
    DynMap,
    newIO,
    lookup,
    insert,
    size,
    sizeIO,
    filter,
    ) where

import Control.Concurrent.STM
       (STM, TVar, atomically, modifyTVar', newTVar, newTVarIO, readTVar,
       readTVarIO, writeTVar)
import Data.Foldable          (traverse_)
import Data.GADT.Compare      (GCompare (..), GEq (..))
import Data.Hashable          (Hashable)
import Futurice.Reflection    (TypeRep, Typeable, typeRep)
import Prelude ()
import Prelude.Compat         hiding (filter, lookup)

import qualified Data.Dependent.Map  as DMap
import qualified Data.HashMap.Strict as HM

-- Families
type family Fst (p :: (*, *)) :: * where
    Fst '(a, b) = a

type family Snd (p :: (*, *)) :: * where
    Snd '(a, b) = b

-- | Dynamic concurrent partial map:
--
-- @forall x. Typeable x => k x -> STM (Maybe (v x))@
newtype DynMap (fk :: * -> *) (fv :: * -> *) = DynMap (TVar (DMap.DMap K (V fk fv)))

-- Key of the outer map
newtype K (p :: (*, *)) = K (TypeRep p)
  deriving Show

instance GEq K where K a `geq` K b = a `geq` b
instance GCompare K where K a `gcompare` K b = a `gcompare` b

-- Value of the outer map, inner map
data V (fk :: * -> *) (fv :: * -> *) (x :: (*, *)) where
    V :: TVar (HM.HashMap (fk (Fst x)) (fv (Snd x))) -> V fk fv x

newIO :: IO (DynMap fk fv)
newIO = DynMap <$> newTVarIO DMap.empty

lookup
    :: forall fk fv k v. (Typeable k, Typeable v, Eq (fk k), Hashable (fk k))
    => fk k -> DynMap fk fv -> STM (Maybe (fv v))
lookup kx (DynMap tvarO) = do
    dmap <- readTVar tvarO
    let k = K typeRep :: K '(k, v)
    case DMap.lookup k dmap of
        Nothing        -> pure Nothing
        Just (V tvarI) -> do
            hm <- readTVar tvarI
            pure (HM.lookup kx hm)

insert
    :: forall fk fv k v. (Typeable k, Typeable v, Eq (fk k), Hashable (fk k))
    => fk k -> fv v -> DynMap fk fv -> STM ()
insert kx vx (DynMap tvarO) = do
    dmap <- readTVar tvarO
    let k = K typeRep :: K '(k, v)
    case DMap.lookup k dmap of
        Nothing -> do
            tvarI <- newTVar (HM.singleton kx vx)
            writeTVar tvarO (DMap.insert k (V tvarI) dmap)
        Just (V tvarI) ->
            modifyTVar' tvarI $ HM.insert kx vx

size :: DynMap fk fv -> STM Int
size = sizeImpl readTVar

-- | Less precise, but doesn't block whole structure.
sizeIO :: DynMap fk fv -> IO Int
sizeIO = sizeImpl readTVarIO

sizeImpl :: Monad m => (forall a. TVar a -> m a) -> DynMap fk fv -> m Int
sizeImpl r (DynMap tvarO) = do
    dmap <- r tvarO
    sum <$> traverse f (DMap.toAscList dmap)
  where
    f (_ DMap.:=> V tvarI) = do
        hm <- r tvarI
        pure (HM.size hm)

filter :: (forall v. fv v -> Bool) -> DynMap fk fv -> IO ()
filter p (DynMap tvarO) = do
    dmap <- readTVarIO tvarO
    traverse_ f (DMap.toAscList dmap)
  where
    f ( _ DMap.:=> V tvarI) = atomically $ modifyTVar' tvarI $ HM.filter p



