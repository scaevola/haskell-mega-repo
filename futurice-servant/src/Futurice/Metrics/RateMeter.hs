module Futurice.Metrics.RateMeter (mark, mark', values) where

import Control.Concurrent.STM
       (TVar, atomically, newTVar, modifyTVar', newTVarIO, readTVar, writeTVar)
import Futurice.Prelude
import Prelude ()

import System.IO.Unsafe (unsafePerformIO)

-------------------------------------------------------------------------------
-- Interface
-------------------------------------------------------------------------------

mark :: Text -> IO ()
mark name = mark' name 1

mark' :: Text -> Word64 -> IO ()
mark' name value = atomically $ do
    gm <- readTVar globalMap
    case gm ^. at name of
        Nothing -> do
            rmTVar <- newTVar
                $ markRateMeter value
                $ zeroRateMeter
            writeTVar globalMap $ gm
                & at name ?~ rmTVar
        Just tvar -> do
            modifyTVar' tvar
                $ markRateMeter value

values :: IO (Map Text Word64)
values = atomically $ readTVar globalMap >>= traverse readMeter
  where
    readMeter tvar = do
        RateMeter value <- readTVar tvar
        writeTVar tvar zeroRateMeter
        return value

-------------------------------------------------------------------------------
-- RateMeter
-------------------------------------------------------------------------------

newtype RateMeter = RateMeter Word64
  deriving Show

zeroRateMeter :: RateMeter
zeroRateMeter = RateMeter 0

markRateMeter :: Word64 -> RateMeter -> RateMeter
markRateMeter value (RateMeter value') = RateMeter (value + value')

-------------------------------------------------------------------------------
-- Internals
-------------------------------------------------------------------------------

-- todo change to be per capacity
globalMap :: TVar (Map Text (TVar RateMeter))
globalMap = unsafePerformIO $ newTVarIO mempty
{-# NOINLINE globalMap #-}
