module Futurice.App.FUM.Transactor where

import Control.Concurrent.MVar.Lifted (withMVar)
import Control.Concurrent.STM
       (atomically, readTVar, writeTChan, writeTVar)
import Futurice.Prelude
import Futurice.Stricter              (runStricterT)
import Prelude ()

import Futurice.App.FUM.Command
import Futurice.App.FUM.Ctx
import Futurice.App.FUM.Types

-- | Write command to 'ctxCommandChananel', and wait for the reply.
--
-- | We want only own writer (i.e. not multiple HTTP-request threads) to
-- ensure consistency of written commands.
transact
    :: Ctx
    -> UTCTime       -- ^ now
    -> Login         -- ^ submitted of the command
    -> SomeCommand   -- ^ command
    -> LogT IO (Either String LomakeResponse)
transact ctx now login scmd =
    withSomeCommand scmd $ \cmd ->
    withMVar (ctxTransactorMVar ctx) $ \_  -> do
        -- logTrace ("command " <> commandTag (Proxy :: Proxy cmd)) cmd
        world <- liftIO $ atomically $ readTVar (ctxWorld ctx)
        case runStricterT (applyCommand now login cmd) world of
            Right (res, world') -> do
                -- TODO: persist
                liftIO $ atomically $ do
                    writeTVar (ctxWorld ctx) world'
                    writeTChan (ctxCommandChannel ctx) scmd
                pure (Right res)
            Left err -> pure (Left err)
