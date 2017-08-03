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

import Control.Concurrent     (threadDelay)
import Control.Concurrent.STM
       (TVar, atomically, newTVarIO, readTVarIO, writeTVar)
import Control.Lens
       (filtered, firstOf, folded, view, (&), (.~), (^.), _Just)
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
    token <- getSequenceToken env group stream
    tokenTVar <- newTVarIO token
    logger <- Log.mkBulkLogger "cloudwatch" (write tokenTVar) (pure ())
    Log.withLogger logger act
  where
    write :: TVar (Maybe Text) -> [Log.LogMessage] -> IO ()
    write _         []     = pure ()
    write tokenTVar (m:ms) = handle retry $ AWS.runResourceT $ AWS.runAWS env $ do
        token <- liftIO $ readTVarIO tokenTVar
        let ple = AWS.putLogEvents group stream (sortNE $ mkEvent <$>  m :| ms)
              & AWS.pleSequenceToken .~ token
        res <- AWS.send ple
        -- write next token
        liftIO . atomically . writeTVar tokenTVar $ res ^. AWS.plersNextSequenceToken
      where
        retry :: SomeException -> IO ()
        retry ex = do
            hPutStrLn stderr $ "CloudWatch: unexpected error: " ++ show ex
            -- wait 10 sec
            threadDelay $ 10 * 1000000
            -- and try to get new token
            newToken <- getSequenceToken env group stream
            atomically . writeTVar tokenTVar $ newToken
            write tokenTVar (m:ms)

    mkEvent :: Log.LogMessage -> AWS.InputLogEvent
    mkEvent lm = AWS.inputLogEvent stamp msg
      where
        -- multiply by 1000, because AWS wants milliseconds
        stamp = truncate $ max 0 $ (1000 *) $ utcTimeToPOSIXSeconds $ Log.lmTime lm
        msg = Log.showLogMessage Nothing lm

    sortNE (x :| xs) = case sortOn (view AWS.ileTimestamp) (x : xs) of
        (y : ys) -> y :| ys
        []       -> error "unexpected"

getSequenceToken :: AWS.Env -> Text -> Text -> IO (Maybe Text)
getSequenceToken env group stream = AWS.runResourceT $ AWS.runAWS env $
    getSequenceToken' group stream

getSequenceToken' :: AWS.MonadAWS m => Text -> Text -> m (Maybe Text)
getSequenceToken' group stream = do
    let dls = AWS.describeLogStreams group
    res <- AWS.send dls
    return $ firstOf sequenceTokens res
  where
    sequenceTokens
        = AWS.dlsrsLogStreams
        . folded
        . filtered (\s -> s ^. AWS.lsLogStreamName == Just stream)
        . AWS.lsUploadSequenceToken
        . _Just
