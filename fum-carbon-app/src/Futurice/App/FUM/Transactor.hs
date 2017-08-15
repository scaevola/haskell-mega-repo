module Futurice.App.FUM.Transactor where

import Futurice.Prelude
import Prelude ()
import Control.Concurrent.STM (atomically, readTVar, writeTVar, writeTChan)
import Control.Concurrent.MVar.Lifted (withMVar)
import Control.Monad.State.Strict (runStateT)

import Futurice.App.FUM.Ctx
import Futurice.App.FUM.Types
import Futurice.App.FUM.Command

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
        case runStateT (applyCommand now login cmd) world of
            Right (res, world') -> do
                -- TODO: persist
                liftIO $ atomically $ do
                    writeTVar (ctxWorld ctx) world'
                    writeTChan (ctxCommandChannel ctx) scmd
                pure (Right res)
            Left err -> pure (Left err)
