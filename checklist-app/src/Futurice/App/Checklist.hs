{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist (defaultMain) where

import Prelude ()
import Futurice.Prelude
import Control.Concurrent.STM    (atomically, readTVarIO, writeTVar)
import Data.Foldable             (foldl')
import Data.Pool                 (withResource)
import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Servant
import Futurice.Stricter
import Servant

import Futurice.App.Checklist.Ack
import Futurice.App.Checklist.API
import Futurice.App.Checklist.Command
import Futurice.App.Checklist.Config
import Futurice.App.Checklist.Markup                (uriText)
import Futurice.App.Checklist.Pages.Checklist
import Futurice.App.Checklist.Pages.Checklists
import Futurice.App.Checklist.Pages.CreateChecklist
import Futurice.App.Checklist.Pages.CreateEmployee
import Futurice.App.Checklist.Pages.CreateTask
import Futurice.App.Checklist.Pages.Employee
import Futurice.App.Checklist.Pages.Error
       (forbiddedPage, notFoundPage)
import Futurice.App.Checklist.Pages.Index
import Futurice.App.Checklist.Pages.Task
import Futurice.App.Checklist.Pages.Tasks
import Futurice.App.Checklist.Types
import Futurice.App.Checklist.Types.Ctx

import qualified Database.PostgreSQL.Simple as Postgres
import qualified FUM                        (UserName (..))

-------------------------------------------------------------------------------
-- Server
-------------------------------------------------------------------------------

server :: Ctx -> Server ChecklistAPI
server ctx = indexPageImpl ctx
    :<|> tasksPageImpl ctx
    :<|> checklistsPageImpl ctx
    :<|> createChecklistPageImpl ctx
    :<|> createTaskPageImpl ctx
    :<|> createEmployeePageImpl ctx
    :<|> checklistPageImpl ctx
    :<|> taskPageImpl ctx
    :<|> employeePageImpl ctx
    :<|> commandImpl ctx

-------------------------------------------------------------------------------
-- Endpoint wrappers
-------------------------------------------------------------------------------

indexPageImpl
    :: (MonadIO m, MonadTime m)
    => Ctx
    -> Maybe FUM.UserName
    -> Maybe Location
    -> Maybe (Identifier Checklist)
    -> Maybe (Identifier Task)
    -> Bool
    -> m (HtmlPage "indexpage")
indexPageImpl ctx fu loc cid tid showAll = withAuthUser ctx fu impl
  where
    impl world userInfo = do
        today <- currentDay
        pure $ indexPage world today userInfo loc checklist task showAll
      where
        checklist = do
            cid' <- cid
            world ^? worldLists . ix cid'

        task = do
            tid' <- tid
            world ^? worldTasks . ix tid'

tasksPageImpl
    :: (MonadIO m)
    => Ctx
    -> Maybe FUM.UserName
    -> Maybe TaskRole
    -> Maybe (Identifier Checklist)
    -> m (HtmlPage "tasks")
tasksPageImpl ctx fu role cid = withAuthUser ctx fu impl
  where
    impl world userInfo =
        pure $ tasksPage world userInfo role checklist
      where
        checklist = do
            cid' <- cid
            world ^? worldLists . ix cid'

createChecklistPageImpl
    :: (MonadIO m)
    => Ctx
    -> Maybe FUM.UserName
    -> m (HtmlPage "create-checklist")
createChecklistPageImpl ctx fu = withAuthUser ctx fu impl
  where
    impl world userInfo = pure $ createChecklistPage world userInfo

createTaskPageImpl
    :: (MonadIO m)
    => Ctx
    -> Maybe FUM.UserName
    -> m (HtmlPage "create-task")
createTaskPageImpl ctx fu = withAuthUser ctx fu impl
  where
    impl world userInfo = pure $ createTaskPage world userInfo

createEmployeePageImpl
    :: (MonadIO m)
    => Ctx
    -> Maybe FUM.UserName
    -> Maybe (Identifier Employee)
    -> m (HtmlPage "create-employee")
createEmployeePageImpl ctx fu meid = withAuthUser ctx fu impl
  where
    impl world userInfo = pure $ createEmployeePage world userInfo memployee
      where
        memployee = meid >>= \eid -> world ^? worldEmployees . ix eid


checklistsPageImpl
    :: MonadIO m
    => Ctx
    -> Maybe FUM.UserName
    -> m (HtmlPage "checklists")
checklistsPageImpl ctx fu = withAuthUser ctx fu impl
  where
    impl world userInfo = pure $ checklistsPage world userInfo

taskPageImpl
    :: (MonadIO m, MonadTime m)
    => Ctx
    -> Maybe FUM.UserName
    -> Identifier Task
    -> m (HtmlPage "task")
taskPageImpl ctx fu tid = withAuthUser ctx fu impl
  where
    impl world userInfo = case world ^? worldTasks . ix tid of
        Nothing   -> pure notFoundPage
        Just task -> do
            today <- currentDay
            pure $ taskPage world today userInfo task

checklistPageImpl
    :: (MonadIO m, MonadTime m)
    => Ctx
    -> Maybe FUM.UserName
    -> Identifier Checklist
    -> m (HtmlPage "checklist")
checklistPageImpl ctx fu cid = withAuthUser ctx fu impl
  where
    impl world userInfo = case world ^? worldLists . ix cid of
        Nothing        -> pure notFoundPage
        Just checklist -> do
            today <- currentDay
            pure $ checklistPage world today userInfo checklist

employeePageImpl
    :: (MonadIO m)
    => Ctx
    -> Maybe FUM.UserName
    -> Identifier Employee
    -> m (HtmlPage "employee")
employeePageImpl ctx fu eid = withAuthUser ctx fu impl
  where
    impl world userInfo = pure $ case world ^? worldEmployees . ix eid of
        Nothing       -> notFoundPage
        Just employee -> employeePage world userInfo employee

-------------------------------------------------------------------------------
-- Command implementation
-------------------------------------------------------------------------------

commandImpl
    :: MonadIO m
    => Ctx
    -> Maybe FUM.UserName
    -> Command Proxy
    -> m Ack
commandImpl ctx fu cmd =
    withAuthUser' (AckErr "forbidden") ctx fu $ \_world (fumUsername, _, _) ->
    liftIO $ runLogT "command" (ctxLogger ctx) $ do
        (cmd', res) <- instantiatedCmd
        ctxApplyCmd fumUsername cmd' ctx
        pure res
  where
    instantiatedCmd = flip runStricterT mempty $ traverseCommand genIdentifier cmd

    genIdentifier
        :: (MonadIO m, MonadWriter Ack m)
        => CIT x -> Proxy (Identifier x) -> m (Identity (Identifier x))
    genIdentifier CITEmployee Proxy = do
        eid <- Identifier <$> ctxGetCRandom ctx
        tell $ AckLoad $ uriText $
            safeLink checklistApi employeePageEndpoint eid
        pure (Identity eid)
    genIdentifier CITTask Proxy = do
        tid <- Identifier <$> ctxGetCRandom ctx
        tell $ AckLoad $ uriText $
            safeLink checklistApi taskPageEndpoint tid
        pure (Identity tid)
    genIdentifier CITChecklist Proxy = do
        cid <- Identifier <$> ctxGetCRandom ctx
        tell $ AckLoad $ uriText $
            safeLink checklistApi checklistPageEndpoint cid
        pure (Identity cid)

-------------------------------------------------------------------------------
-- Auth
-------------------------------------------------------------------------------

-- | Read only pages
withAuthUser
    :: MonadIO m
    => Ctx -> Maybe FUM.UserName
    -> (World -> AuthUser -> m (HtmlPage a))
    -> m (HtmlPage a)
withAuthUser = withAuthUser' forbiddedPage

withAuthUser'
    :: MonadIO m
    => a                           -- ^ Response to unauthenticated users
    -> Ctx -> Maybe FUM.UserName
    -> (World -> AuthUser -> m a)
    -> m a
withAuthUser' def ctx fu f = do
    world <- liftIO $ readTVarIO $ ctxWorld ctx
    case userInfo world of
        Nothing        -> pure def
        Just userInfo' -> f world userInfo'
  where
    userInfo :: World -> Maybe (FUM.UserName, TaskRole, Location)
    userInfo world = world ^? worldUsers . ix fu . _Just

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

defaultMain :: IO ()
defaultMain = futuriceServerMain makeCtx $ emptyServerConfig
    & serverName             .~ "Checklist"
    & serverDescription      .~ "Super TODO"
    & serverColour           .~ (Proxy :: Proxy ('FutuAccent 'AF4 'AC3))
    & serverApp checklistApi .~ server
    & serverEnvPfx           .~ "CHECKLISTAPP"
  where
    mockCredentials = (FUM.UserName "phadej", TaskRoleIT, LocHelsinki)

    makeCtx :: Config -> Logger -> DynMapCache -> IO Ctx
    makeCtx cfg logger _cache = do
        ctx <- newCtx logger (cfgPostgresConnInfo cfg) emptyWorld
        cmds <- withResource (ctxPostgres ctx) $ \conn ->
            Postgres.fromOnly <$$> Postgres.query_ conn "SELECT cmddata FROM checklist2.commands ORDER BY cid;"
        let world0 = foldl' (flip applyCommand) emptyWorld cmds
        let world1 = if cfgMockAuth cfg
            then world0 & worldUsers .~ const (Just mockCredentials)
            else world0
        atomically $ writeTVar (ctxWorld ctx) world1
        pure ctx
