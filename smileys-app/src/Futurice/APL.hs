{-# LANGUAGE ScopedTypeVariables #-}
module Futurice.APL where

import Data.Functor.Rep
import Futurice.Prelude
import Linear
       (E (..), V0 (..), V1 (..), V2 (..), V3 (..), V4 (..), _w, _x, _y, _z)
import Prelude ()

-------------------------------------------------------------------------------
-- Naperian
-------------------------------------------------------------------------------

-- | Essentially a 'Representable', but also require -- bounds on @'Rep' f@.
class (Representable f, Traversable f) => Naperian f where
    positions :: f (Rep f)

instance Naperian V0 where positions = V0
instance Naperian V1 where positions = V1 (E _x)
instance Naperian V2 where positions = V2 (E _x) (E _y)
instance Naperian V3 where positions = V3 (E _x) (E _y) (E _z)
instance Naperian V4 where positions = V4 (E _x) (E _y) (E _z) (E _w)

-------------------------------------------------------------------------------
-- APL
-------------------------------------------------------------------------------

newtype APL f a = APL { runAPL :: Rep f -> a }

instance Functor (APL f) where
    fmap f (APL x) = APL (fmap f x)

instance Naperian f => Foldable (APL f) where
    foldMap f (APL x) = foldMap (f . x) (positions :: f (Rep f))

instance Naperian f => Traversable (APL f) where
    traverse f = fmap toAPL . traverse f . fromAPL

instance Applicative (APL f) where
    pure = APL . pure
    APL f <*> APL x = APL (f <*> x)

-- instance Monad

instance Semigroup a => Semigroup (APL f a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (APL f a) where
    mempty = pure mempty
    mappend = liftA2 mappend

-------------------------------------------------------------------------------
-- Conversion
-------------------------------------------------------------------------------

toAPL :: Representable f => f a -> APL f a
toAPL =  APL . index

fromAPL :: Representable f => APL f a -> f a
fromAPL = tabulate . runAPL
