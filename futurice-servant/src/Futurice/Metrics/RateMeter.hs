module Futurice.Metrics.RateMeter (mark, mark', values) where

import Control.Concurrent.STM
       (TVar, atomically, newTVar, modifyTVar', newTVarIO, readTVar, writeTVar)
import Futurice.Prelude
import Prelude ()

import qualified Data.Vector.Unboxed as U

import System.IO.Unsafe (unsafePerformIO)

-------------------------------------------------------------------------------
-- Interface
-------------------------------------------------------------------------------

mark :: Text -> IO ()
mark name = mark' name 1

mark' :: Text -> Int64 -> IO ()
mark' name value = do
    TimeSpec seconds _ <- monotonicClock
    let minutes = seconds `div` 60

    atomically $ do
        gm <- readTVar globalMap
        case gm ^. at name of
            Nothing -> do
                rmTVar <- newTVar
                    $ markRateMeter minutes value
                    $ RateMeter (U.replicate 60 0) minutes
                writeTVar globalMap $ gm
                    & at name ?~ rmTVar
            Just tvar -> modifyTVar' tvar
                $ markRateMeter minutes value
                . advanceRateMeter minutes

values :: IO (Map Text Int64)
values = do
    TimeSpec seconds _ <- monotonicClock
    let minutes = seconds `div` 60
    meters <- atomically $ readTVar globalMap >>= traverse readTVar
    pure (averageOfRateMeter . advanceRateMeter minutes <$> meters)

-------------------------------------------------------------------------------
-- RateMeter
-------------------------------------------------------------------------------

data RateMeter = RateMeter
    { _rmVec :: !(U.Vector Int64)
    , _rmMin :: !Int64
    }
  deriving Show

markRateMeter :: Int64 -> Int64 -> RateMeter -> RateMeter
markRateMeter minutes' value (RateMeter vec minutes) = RateMeter (U.imap f vec) minutes
  where
    idx = fromIntegral (minutes' `mod` 60) :: Int
    f i x | i == idx  = x + value
          | otherwise = x

advanceRateMeter :: Int64 -> RateMeter -> RateMeter
advanceRateMeter minutes' rm@(RateMeter vec minutes)
    | minutes == minutes' = rm
    | otherwise = RateMeter vec' minutes'
  where
    vec' = U.imap f vec
    offset = (minutes `div` 60) * 60
    f i x | minutes < offset + i' && offset + i' <= minutes' = 0
          | otherwise                                        = x
      where
        i' = fromIntegral i

averageOfRateMeter :: RateMeter -> Int64
averageOfRateMeter (RateMeter vec _) = U.sum vec

-------------------------------------------------------------------------------
-- Internals
-------------------------------------------------------------------------------

-- todo change to be per capacity
globalMap :: TVar (Map Text (TVar RateMeter))
globalMap = unsafePerformIO $ newTVarIO mempty
{-# NOINLINE globalMap #-}
