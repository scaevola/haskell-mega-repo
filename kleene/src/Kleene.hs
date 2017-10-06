{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Kleene where

import Prelude ()
import Prelude.Compat

import Control.Applicative (Alternative (..), liftA2)
import Data.RangeSet.List  (RSet)
import Data.String         (IsString (..))
import Data.Text           (Text)
import Data.Semigroup (Semigroup (..))

import qualified Data.RangeSet.List     as RSet
import qualified Text.Regex.Applicative as R

data Greediness = Greedy | NonGreedy
  deriving (Eq, Ord, Show, Enum, Bounded)

data Kleene c a where
    KleeneEmpty  :: Kleene c a
    KleenePure   :: a -> Kleene c a
    KleeneChar   :: (Ord c, Enum c) => RSet c -> Kleene c c
    KleeneAppend :: (a -> b -> r) -> Kleene c a -> Kleene c b -> Kleene c r
    KleeneUnion  :: Kleene c a -> Kleene c a -> Kleene c a
    KleeneStar   :: Greediness -> Kleene c a -> Kleene c [a]

    -- optimisations
    KleeneMap    :: (a -> b) -> Kleene c a -> Kleene c b -- could use Pure and  Append
    KleeneString :: Eq c => [c] -> Kleene c [c]          -- could use Char and Append

instance (c ~ Char, IsString a) => IsString (Kleene c a) where
    fromString s = KleeneMap fromString (KleeneString s)

instance Semigroup (Kleene c a) where
    (<>) = (<|>)

instance Monoid (Kleene c a) where
    mempty = empty
    mappend = (<>)

instance Functor (Kleene c) where
    fmap _ KleeneEmpty          = KleeneEmpty
    fmap f (KleenePure x)       = KleenePure (f x)
    fmap f (KleeneMap g k)      = KleeneMap (f . g) k
    fmap f (KleeneAppend g a b) = KleeneAppend (\x y -> f (g x y)) a b
    fmap f k                    = KleeneMap f k

instance Applicative (Kleene c) where
    pure = KleenePure

    KleeneEmpty <*> _ = KleeneEmpty
    _ <*> KleeneEmpty = KleeneEmpty

    KleenePure f <*> k = fmap f k
    k <*> KleenePure x = fmap ($x) k

    f <*> x = KleeneAppend ($) f x
    -- TODO: re-associate trees?

instance Alternative (Kleene c) where
    empty = KleeneEmpty

    KleeneEmpty <|> k = k
    k <|> KleeneEmpty = k
    KleeneChar a <|> KleeneChar b = KleeneChar (RSet.union a b)

    a <|> b = KleeneUnion a b

    many KleeneEmpty      = KleenePure []
    many (KleeneStar _ k) = KleeneMap pure (KleeneStar Greedy k)
    many k                = KleeneStar Greedy k

    some KleeneEmpty      = KleeneEmpty
    some (KleeneStar _ k) = KleeneMap pure (KleeneStar Greedy k)
    some k                = liftA2 (:) k (KleeneStar Greedy k)

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

-- | >>> T.putStrLn $ kleeneToJS kleeneAnyChar
-- ^[^]$
kleeneAnyChar :: (Ord c, Enum c, Bounded c) => Kleene c c
kleeneAnyChar = KleeneChar RSet.full

-- | >>> T.putStrLn $ kleeneToJS kleeneDotChar
-- ^.$
kleeneDotChar :: Kleene Char Char
kleeneDotChar = KleeneChar dotRSet

-- | >>> T.putStrLn $ kleeneToJS kleeneEverything
-- ^[^]*$
kleeneEverything :: (Ord c, Enum c, Bounded c) => Kleene c [c]
kleeneEverything = many kleeneAnyChar

-- | >>> T.putStrLn $ kleeneToJS kleeneEverything1
-- ^[^][^]*$
kleeneEverything1 :: (Ord c, Enum c, Bounded c) => Kleene c [c]
kleeneEverything1 = some kleeneAnyChar

-- | Matches whole input?
--
-- TODO: this is quick test, not proper decision procedure
isKleeneEverything :: Bounded c => Kleene c a -> Bool
isKleeneEverything k = case k of
    KleeneMap _ (KleeneStar _ (KleeneChar cs)) -> cs == RSet.full
    KleeneStar _ (KleeneChar cs)               -> cs == RSet.full
    _                                          -> False

-------------------------------------------------------------------------------
-- regex-applicative
-------------------------------------------------------------------------------

-- | Convert 'Kleene' to 'RE' from @regex-applicative@.
kleeneToRe :: Kleene c a -> R.RE c a
kleeneToRe KleeneEmpty              = empty
kleeneToRe (KleenePure x)           = pure x
kleeneToRe (KleeneChar cs)          = R.psym (\c -> RSet.member c cs)
kleeneToRe (KleeneAppend f a b)     = liftA2 f (kleeneToRe a) (kleeneToRe b)
kleeneToRe (KleeneUnion a b)        = kleeneToRe a <|> kleeneToRe b
kleeneToRe (KleeneStar Greedy a)    = many (kleeneToRe a)
kleeneToRe (KleeneStar NonGreedy a) = R.few (kleeneToRe a)
kleeneToRe (KleeneMap f a)          = fmap f (kleeneToRe a)
kleeneToRe (KleeneString s)         = R.string s

-------------------------------------------------------------------------------
-- JavaScript
-------------------------------------------------------------------------------

-- | Convert to non-matching JavaScript string which can be used
-- as an argument to 'new RegExp'
--
-- TODO: use builder
-- TODO: escaping
--
-- >>> T.putStrLn $ kleeneToJS "foobar"
-- ^foobar$
--
-- >>> T.putStrLn $ kleeneToJS $ many "foobar"
-- ^(?:foobar)*$
--
kleeneToJS :: Kleene Char a -> Text
kleeneToJS kl = "^" <> go False kl <> "$"
  where
    go :: Bool -> Kleene Char b -> Text
    go p (KleeneMap _ k)          = go p k
    go _ (KleeneUnion a b)        = "(?:" <> go True a <> "|"  <> go True b <> ")"
    go p (KleeneAppend _ a b)     = parens p $ go False a <> go False b
    go p (KleeneStar Greedy k)    = parens p $ go True k <> "*"
    go p (KleeneStar NonGreedy k) = parens p $ go True k <> "*?"
    go p (KleeneString s)         = parens p $ fromString s

    go _ KleeneEmpty    = "[]"
    go _ (KleenePure _) = ""
    go _ (KleeneChar cs)
        | cs == RSet.full = "[^]"
        | cs == dotRSet   = "."
        -- this is dangerous...
        -- todo: also use range-size!
        | otherwise     = "[" <> fromString (RSet.toList cs) <> "]"

    parens True t  = "(?:" <> t <> ")"
    parens False t = t

-------------------------------------------------------------------------------
-- Character sets
-------------------------------------------------------------------------------

dotRSet :: RSet Char
dotRSet = RSet.full RSet.\\ RSet.singleton '\n'

-------------------------------------------------------------------------------
-- Doctest
-------------------------------------------------------------------------------

-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Text.IO as T
