{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | You are probably looking for 'cachedIO'
module Futurice.Cache (
    Cache(..),
    newCache,
    cached,
    genCached,
    cachedIO,
    genCachedIO,
    CachePolicy (..),
    cacheSize,
    cleanupCache,
    ) where

import Control.Concurrent.Async    (Async, async, wait, waitCatchSTM)
import Control.Concurrent.STM      (STM, atomically)
import Control.DeepSeq             (NFData)
import Control.Exception           (SomeException, evaluate)
import Control.Monad               (when)
import Control.Monad.Base          (MonadBase (..))
import Control.Monad.Catch         (throwM)
import Control.Monad.Time          (MonadTime (..))
import Control.Monad.Trans.Control (MonadBaseControl (..))
import Data.Functor.Identity       (Identity (..))
import Data.Hashable               (Hashable (..))
import Data.NF                     (NF, getNF, makeNF)
import Data.Time                   (NominalDiffTime, UTCTime, addUTCTime)
import Data.Typeable               (Typeable)
import Log                         (Logger, MonadLog, logAttention_, runLogT)
import Prelude ()
import Prelude.Compat

import qualified Data.Text       as T
import qualified Futurice.DynMap as DynMap

data E x = L !(Async x) | R !x

data Leaf v = Leaf !UTCTime !(E (NF v))

newtype Cache = Cache { _getCache :: DynMap.DynMap Identity Leaf }

-- | Create new cache
newCache :: MonadBase IO m => m Cache
newCache = liftBase (Cache <$> DynMap.newIO)

-- | Update cache
insertCache
    :: forall k v. (Typeable k, Typeable v, Eq k, Hashable k)
    => Cache    -- cahce
    -> k        -- ^ key
    -> UTCTime  -- ^ expiration moment
    -> E (NF v) -- ^ value
    -> STM ()
insertCache (Cache dynmap) k stamp v =
    DynMap.insert (Identity k) (Leaf stamp v) dynmap

-- | Lookup in cache
lookupCache
    :: forall k v. (Typeable k, Typeable v, Eq k, Hashable k)
    => Cache
    -> k
    -> STM (Maybe (Leaf v))
lookupCache (Cache dynmap) k =
    DynMap.lookup (Identity k) dynmap

-- | Approximate size of the cache.
cacheSize :: MonadBase IO m => Cache -> m Int
cacheSize (Cache dynmap) = liftBase $ DynMap.sizeIO dynmap

data CachePolicy
    = ReturnOld   -- ^ Return elements even outdated, populate cache with new element in background
    | RequestNew  -- ^ If element is outdated, request new and return it. /May block/
    deriving (Eq, Show, Typeable)

liftedAsync :: (MonadBaseControl IO m, StM m a ~ a, NFData a) => m a -> m (Async (NF a))
liftedAsync action = liftBaseWith $ \runInIO -> async $ do
    x <- runInIO action
    evaluate (makeNF x)

liftedAtomically :: MonadBase IO m => STM a -> m a
liftedAtomically = liftBase . atomically

waitE :: MonadBase IO m => E (NF x) -> m x
waitE (R x) = pure (getNF x)
waitE (L a) = fmap getNF $ liftBase $ wait a

-------------------------------------------------------------------------------
-- Cached actions
-------------------------------------------------------------------------------

-- | Cached action. Doesn't suffer from cold cache ....
--
-- Simple example:
--
-- @
-- c <- mkCache
-- cachedIO c 10 'k' (putStrLn "foo" >> pure 'v')
-- @
cachedIO
    :: forall k v. (Typeable k, Eq k, Hashable k, Typeable v, NFData v)
    => Logger           -- ^ logger
    -> Cache            -- ^ Cache
    -> NominalDiffTime  -- ^ Time-to-live
    -> k                -- ^ Cache key
    -> IO v             -- ^ Value action
    -> IO v
cachedIO = genCachedIO ReturnOld

-- | Cached action.
--
-- Simple example:
--
-- @
-- c <- mkCache
-- cached c 10 'k' (putStrLn "foo" >> pure 'v')
-- @
cached :: forall k v m.
          ( Typeable k, Eq k, Hashable k, Typeable v, NFData v
          , MonadBaseControl IO m, MonadLog m, StM m v ~ v
          )
       => Cache            -- ^ Cache
       -> NominalDiffTime  -- ^ Time-to-live
       -> k                -- ^ Cache key
       -> m v              -- ^ Value action
       -> m v
cached = genCached ReturnOld

-- | More general version of 'IO' of 'genCached'.
genCachedIO
    :: forall k v. (Typeable k, Eq k, Hashable k, Typeable v, NFData v)
    => CachePolicy
    -> Logger
    -> Cache            -- ^ Cache
    -> NominalDiffTime  -- ^ Time-to-live
    -> k                -- ^ Cache key
    -> IO v             -- ^ Value action
    -> IO v
genCachedIO policy lgr cache ttl k action =
    runLogT "futurice-cache" lgr $
        genCached policy cache ttl k (liftBase action)

genCached
    :: forall k v m.
        ( Typeable k, Eq k, Hashable k
        , Typeable v, NFData v
        , MonadLog m, MonadTime m, MonadBaseControl IO m, StM m v ~ v
        )
    => CachePolicy
    -> Cache            -- ^ Cache
    -> NominalDiffTime  -- ^ Time-to-live
    -> k                -- ^ Cache key
    -> m v             -- ^ Value action
    -> m v
genCached policy cache ttl key action = do
    now <- liftBase $ currentTime
    let expirationMoment = ttl `addUTCTime` now
    -- Errors are cached for 60 seconds
    let errorExpirationMoment = 60 `addUTCTime` now

    -- Lookup
    ro <- liftedAtomically $ do
        r <- lookupCache cache key
        case r of
            Nothing -> pure Nothing
            Just (Leaf stamp e)
                -- If data in cache and recent enough, use it
                | stamp > now -> pure (Just (e, False))
                -- otherwise, re-insert it with new (error) timestamp so concurrent
                -- requests will still reuse old value.  But return
                -- nothing, so this request will be handled by the actual
                -- application.
                | otherwise -> case policy of
                    RequestNew -> pure Nothing
                    ReturnOld  -> do
                        insertCache cache key errorExpirationMoment e
                        pure (Just (e, True))

    --  Action, which updates the cache
    let cachedAction :: m ()
        cachedAction = do
            r'' <- liftedAsync action :: m (Async (NF v))
            postAction <- liftedAtomically $ do
                r' <- waitCatchSTM r'' :: STM (Either SomeException (NF v))
                case r' of
                    Left exc -> pure $
                        logAttention_ $ T.pack $ "Exception: " ++ show exc
                    Right x   -> do
                        insertCache cache key expirationMoment (R x)
                        pure (pure ())
            postAction

    -- Initial action, we insert it immediately with short time
    -- if it succeeds we reinsert it with longer moment
    let cachedActionInitial :: m v
        cachedActionInitial = getNF <$> do
            a <- liftedAsync action
            -- insert immediately
            liftedAtomically $ insertCache cache key errorExpirationMoment (L a)
            -- wait, if succeeds extend the ttl
            liftedAtomically $ do
                e <- waitCatchSTM a
                case e of
                    Left exc  -> throwM exc
                    Right x -> do
                        insertCache cache key expirationMoment (R x)
                        pure x

    -- If we got response from cache use it.
    -- If it's old, renew it asynchronously
    case ro of
        Nothing -> cachedActionInitial
        Just (e, needUpdate) -> do
            when needUpdate cachedAction
            waitE e

-------------------------------------------------------------------------------
-- Cleanup
-------------------------------------------------------------------------------

cleanupCache
    :: MonadBase IO m
    => Cache   -- ^ cache
    -> UTCTime -- ^ cut off moment, elements older than that are removed
    -> m ()
cleanupCache (Cache dynmap) stamp = liftBase $ DynMap.filter p dynmap
  where
    p :: Leaf v -> Bool
    p (Leaf stamp' _) = stamp' > stamp
    -- keep elements with @stamp'@ larger than given @stamp@
