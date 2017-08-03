{-# LANGUAGE OverloadedStrings #-}
-- |
-- @
-- env <- AWS.newEnv creds
-- withCloudWatchLogger env "my group" "my stream" token $ \logger ->
--     Log.runLogT "test" logger $ Log.logTrace_ "foobar"
-- @
--
-- AWS user should be able to perform
-- @ "logs:PutLogEvents"@ and @"logs:DescribeLogStreams" actions.
--
module Log.Backend.CloudWatchLogs (withCloudWatchLogger) where

import Control.Concurrent.STM
       (TVar, atomically, newTVarIO, readTVarIO, writeTVar)
import Control.Lens           (filtered, folded, view, (&), (?~), (^.), _Just)
import Control.Monad.IO.Class (liftIO)
import Data.List.Compat       (sortOn)
import Data.List.NonEmpty     (NonEmpty (..))
import Data.Text              (Text)
import Data.Time.Clock.POSIX  (utcTimeToPOSIXSeconds)
import Prelude ()
import Prelude.Compat
import System.IO              (hPutStrLn, stderr)

import qualified Log
import qualified Log.Internal.Logger                           as Log
import qualified Network.AWS                                   as AWS
import qualified Network.AWS.CloudWatchLogs.DescribeLogStreams as AWS
import qualified Network.AWS.CloudWatchLogs.PutLogEvents       as AWS
import qualified Network.AWS.CloudWatchLogs.Types              as AWS

import Control.Exception (SomeException, handle)

withCloudWatchLogger
    :: AWS.Env               -- ^ AWS Environment
    -> Text                  -- ^ group name
    -> Text                  -- ^ stream name
    -> (Log.Logger -> IO r)  -- ^ action
    -> IO r
withCloudWatchLogger env group stream act = do
    -- TODO: Token handling is ugly as ...
    tokenTVar <- newTVarIO ""
    logger <- Log.mkBulkLogger "cloudwatch" (write tokenTVar) (pure ())
    Log.withLogger logger act
  where
    write :: TVar Text -> [Log.LogMessage] -> IO ()
    write _         []     = pure ()
    write tokenTVar (m:ms) = handle retry $ AWS.runResourceT $ AWS.runAWS env $ do
        token' <- liftIO $ readTVarIO tokenTVar
        token <- case token' of
            _ | token' /= mempty -> return token'
              | otherwise -> do
                -- if token is empty...
                -- we rely on the fact mempty @Text == "", that's ugly
                let dls = AWS.describeLogStreams group
                res <- AWS.send dls
                return $ res ^. AWS.dlsrsLogStreams
                    . folded
                    . filtered (\s -> s ^. AWS.lsLogStreamName == Just stream)
                    . AWS.lsUploadSequenceToken
                    . _Just

        let ple = AWS.putLogEvents group stream (s $ mkEvent <$>  m :| ms)
              & AWS.pleSequenceToken ?~ token
        res <- AWS.send ple
        liftIO $ atomically $
            writeTVar tokenTVar (res ^. AWS.plersNextSequenceToken . _Just)

      where
        retry :: SomeException -> IO ()
        retry ex = do
            hPutStrLn stderr $ "CloudWatch: unexpecter error: " ++ show ex

    mkEvent :: Log.LogMessage -> AWS.InputLogEvent
    mkEvent lm = AWS.inputLogEvent stamp msg
      where
        -- multiply by 1000, because AWS wants milliseconds
        stamp = truncate $ max 0 $ (1000 *) $ utcTimeToPOSIXSeconds $ Log.lmTime lm
        msg = Log.showLogMessage Nothing lm

    s (x :| xs) = case sortOn (view AWS.ileTimestamp) (x : xs) of
        (y : ys) -> y :| ys
        []       -> error "unexpected"
