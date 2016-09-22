{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
-- TODO: remove no-orphans
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Futurice.App.Checklist.Types.Identifier (
    Identifier (..),
    identifierToText,
    HasIdentifier (..),
    ) where

import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import Control.Lens (Iso', iso)

import qualified Data.UUID       as UUID

newtype Identifier a = Identifier UUID
    deriving (Eq, Ord, Show, Typeable, Generic)

identifierToText :: Identifier a -> Text
identifierToText (Identifier uuid) = UUID.toText uuid

instance Arbitrary (Identifier a) where
    arbitrary = Identifier <$> arbitrary

-------------------------------------------------------------------------------
-- HasIdentifier
-------------------------------------------------------------------------------

class HasIdentifier entity ident | entity -> ident where
    identifier :: Lens' entity (Identifier ident)

instance HasIdentifier (Identifier e) e where
    identifier = id

-------------------------------------------------------------------------------
-- Orphans: move to futurice-prelude
-------------------------------------------------------------------------------

uuidWords :: Iso' UUID (Word32, Word32, Word32, Word32)
uuidWords = iso UUID.toWords fromWords'
  where
    fromWords' :: (Word32, Word32, Word32, Word32) -> UUID
    fromWords' (a, b, c, d) = UUID.fromWords a b c d

instance Arbitrary UUID where
    arbitrary = view (from uuidWords) <$> arbitrary
    shrink = fmap (view $ from uuidWords) . shrink . view uuidWords