{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.FUM.Pages.Server (pagesServer) where

import Futurice.Prelude
import Prelude ()
import Servant          ((:<|>) (..), Handler, Server)

import Futurice.App.FUM.API.Pages
import Futurice.App.FUM.Ctx
import Futurice.App.FUM.Markup
import Futurice.App.FUM.Pages.CreateEmployee
import Futurice.App.FUM.Pages.CreateGroup
import Futurice.App.FUM.Pages.FromPersonio
import Futurice.App.FUM.Pages.Index
import Futurice.App.FUM.Pages.ListEmployees
import Futurice.App.FUM.Pages.ListGroups
import Futurice.App.FUM.Pages.Summary
import Futurice.App.FUM.Pages.ViewEmployee
import Futurice.App.FUM.Pages.ViewGroup
import Futurice.App.FUM.Types

import qualified Futurice.IdMap as IdMap
import qualified Personio

import Futurice.App.FUM.Auth
import Futurice.App.FUM.Pages.Error

pagesServer :: Ctx -> Server FumCarbonPagesApi
pagesServer ctx = indexPageImpl ctx
    :<|> fromPersonioPageImpl ctx
    :<|> listEmployeesPageImpl ctx
    :<|> viewEmployeePageImpl ctx
    :<|> createEmployeePageImpl ctx
    :<|> listGroupsPageImpl ctx
    :<|> viewGroupPageImpl ctx
    :<|> createGroupPageImpl ctx
    :<|> summaryPageImpl ctx

-------------------------------------------------------------------------------
-- Index
-------------------------------------------------------------------------------

indexPageImpl
    :: Ctx
    -> Maybe Login
    -> Handler (HtmlPage "indexpage")
indexPageImpl ctx mfu = withAuthUser ctx mfu $ \auth world es ->
    pure $ indexPage auth world es

-- TODO: acl
fromPersonioPageImpl
    :: Ctx
    -> Maybe Login
    -> Handler (HtmlPage "from-personio")
fromPersonioPageImpl ctx mfu = withAuthUser ctx mfu $ \auth world es ->
    pure $ fromPersonioPage auth world es

-------------------------------------------------------------------------------
-- Employees
-------------------------------------------------------------------------------

listEmployeesPageImpl
    :: Ctx
    -> Maybe Login
    -> Handler (HtmlPage "list-employees")
listEmployeesPageImpl ctx mfu = withAuthUser ctx mfu $ \auth world _personio ->
    pure $ listEmployeesPage auth world

viewEmployeePageImpl
    :: Ctx
    -> Maybe Login
    -> Login
    -> Handler (HtmlPage "view-employee")
viewEmployeePageImpl ctx fu login = withAuthUser ctx fu $ \auth world personio ->
    case world ^? worldEmployees . ix login of
        Just e  -> do
            now <- currentTime
            pure $ viewEmployeePage auth world personio now e
        Nothing -> pure $ notFoundPage auth $
            "Cannot find user " <> loginToText login

createEmployeePageImpl
    :: Ctx
    -> Maybe Login
    -> Personio.EmployeeId
    -> Handler (HtmlPage "create-employee")
createEmployeePageImpl ctx fu pid = withAuthUser ctx fu $ \auth world personio ->
    case personio ^? ix pid of
        Just e -> pure $ createEmployeePage auth world personio e
        Nothing -> pure $ notFoundPage auth $
            "No such personio id " <> textShow pid

-------------------------------------------------------------------------------
-- Groups
-------------------------------------------------------------------------------

listGroupsPageImpl
    :: Ctx
    -> Maybe Login
    -> Handler (HtmlPage "list-groups")
listGroupsPageImpl ctx mfu = withAuthUser ctx mfu $ \auth world _personio ->
    pure $ listGroupsPage auth world

viewGroupPageImpl
    :: Ctx
    -> Maybe Login
    -> GroupName
    -> Handler (HtmlPage "view-group")
viewGroupPageImpl ctx fu gn = withAuthUser ctx fu $ \auth world personio ->
    case world ^? worldGroups . ix gn of
        Just g  -> pure $ viewGroupPage auth world personio g
        Nothing -> pure $ notFoundPage auth $
            "Cannot find group: " <> groupNameToText gn

createGroupPageImpl
    :: Ctx
    -> Maybe Login
    -> Handler (HtmlPage "create-group")
createGroupPageImpl ctx fu = withAuthUser ctx fu $ \auth world _personio ->
    pure $ createGroupPage auth world

-------------------------------------------------------------------------------
-- It
-------------------------------------------------------------------------------

summaryPageImpl
    :: Ctx
    -> Maybe Login
    -> Handler (HtmlPage "summary")
summaryPageImpl ctx fu = withAuthUser ctx fu $ \auth world personio ->
    pure $ summaryPage auth world personio

-------------------------------------------------------------------------------
-- Auth
-------------------------------------------------------------------------------

-- | Read only pages
withAuthUser
    :: Ctx
    -> Maybe Login
    -> (AuthUser -> World -> IdMap.IdMap Personio.Employee -> Handler (HtmlPage a))
    -> Handler (HtmlPage a)
withAuthUser ctx mfu f = runLogT "page" (ctxLogger ctx) $
    withAuthUser' (forbiddenPage Nothing) ctx mfu $ \auth world personio ->
        if hasRights auth
        then lift $ f auth world personio
        else return $ forbiddenPage (Just (authLogin auth, world, personio))
