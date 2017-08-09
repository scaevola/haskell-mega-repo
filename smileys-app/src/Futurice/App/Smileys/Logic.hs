{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Smileys.Logic (
    getSmileys,
    getOwnSmileys,
    postOwnSmileys,
    ) where

import Data.Pool        (withResource)
import FUM.Types.Login  (Login)
import Futurice.Prelude
import Prelude ()
import Servant          (ServantErr (..), err403)

import Futurice.App.Smileys.Ctx
import Futurice.App.Smileys.Types

import qualified Database.PostgreSQL.Simple as Postgres

getOwnSmileys
    :: (MonadIO m, MonadBaseControl IO m, MonadTime m, MonadError ServantErr m)
    => Ctx
    -> Maybe Login
    -> Maybe Day
    -> Maybe Day
    -> m [Smileys]
getOwnSmileys ctx mfum start end =
    mcase (mfum <|> ctxMockUser ctx) (throwError err403) $ \fumUsername ->
        withResource (ctxPostgresPool ctx) $ \conn -> do
            today <- currentDay
            getSmileysImpl conn (fromMaybe today start) (fromMaybe today end) (Just fumUsername)

getSmileys
    :: (MonadIO m, MonadBaseControl IO m, MonadTime m)
    => Ctx
    -> Maybe Day
    -> Maybe Day
    -> Maybe Login
    -> m [Smileys]
getSmileys ctx start end mFumUsername =
    withResource (ctxPostgresPool ctx) $ \conn -> do
        today <- currentDay
        getSmileysImpl conn (fromMaybe today start) (fromMaybe today end) mFumUsername

getSmileysImpl
    :: MonadIO m
    => Postgres.Connection
    -> Day
    -> Day
    -> Maybe Login
    -> m [Smileys]
getSmileysImpl conn s e Nothing = liftIO $ Postgres.query conn
    "SELECT entries, username, smiley, day FROM smileys.trail WHERE day >= ? AND day <= ?;"
    (s, e)
getSmileysImpl conn s e (Just fumUsername) = liftIO $ Postgres.query conn
    "SELECT entries, username, smiley, day FROM smileys.trail WHERE day >= ? AND day <= ? AND username = ?;"
    (s, e, fumUsername)

postOwnSmileys
    :: (MonadIO m, MonadBaseControl IO m, MonadError ServantErr m)
    => Ctx
    -> Maybe Login
    -> PostSmiley
    -> m Res
postOwnSmileys ctx mfum req =
    mcase (mfum <|> ctxMockUser ctx) (throwError err403) $ \fumUsername ->
        withResource (ctxPostgresPool ctx) $ \conn -> do
            let insertQuery = fromString $ unwords
                 [ "INSERT INTO smileys.trail as c (entries, username, smiley, day)"
                 , "VALUES (?, ?, ?, ?) ON CONFLICT (username, day) DO UPDATE"
                 , "SET entries = EXCLUDED.entries, smiley = EXCLUDED.smiley"
                 ]

            _ <- liftIO $ Postgres.execute conn insertQuery Smileys
                  { _smileysEntries  = _postSmileyEntries req
                  , _smileysUsername = fumUsername
                  , _smileysDate     = _postSmileyDate req
                  , _smileysSmiley   = _postSmileySmiley req
                  }
            pure Res
                { _resStatus = "OK"
                , _resUnused = ()
                }
