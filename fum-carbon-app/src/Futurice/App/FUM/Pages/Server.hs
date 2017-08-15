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
import Futurice.App.FUM.Pages.Index
import Futurice.App.FUM.Pages.ListEmployees
import Futurice.App.FUM.Pages.ViewEmployee
import Futurice.App.FUM.Types

import qualified Futurice.IdMap as IdMap
import qualified Personio

import Futurice.App.FUM.Auth
import Futurice.App.FUM.Pages.Error

pagesServer :: Ctx -> Server FumCarbonPagesApi
pagesServer ctx = indexPageImpl ctx
    :<|> listEmployeesPageImpl ctx
    :<|> viewEmployeePageImpl ctx
    :<|> createEmployeePageImpl ctx

-------------------------------------------------------------------------------
-- Index
-------------------------------------------------------------------------------

indexPageImpl
    :: Ctx
    -> Maybe Login
    -> Handler (HtmlPage "indexpage")
indexPageImpl ctx mfu = withAuthUser ctx mfu $ \auth world es ->
    pure $ indexPage auth world es

-------------------------------------------------------------------------------
-- Employees
-------------------------------------------------------------------------------

createEmployeePageImpl
    :: Ctx
    -> Maybe Login
    -> Personio.EmployeeId
    -> Handler (HtmlPage "create-employee")
createEmployeePageImpl ctx fu pid = withAuthUser ctx fu $ \auth world es ->
    case es ^? ix pid of
        Just e -> pure $ createEmployeePage auth world es e
        Nothing -> pure $ notFoundPage auth $
            "No such personio id " <> textShow pid

viewEmployeePageImpl
    :: Ctx
    -> Maybe Login
    -> Login
    -> Handler (HtmlPage "view-employee")
viewEmployeePageImpl ctx fu login = withAuthUser ctx fu $ \auth world _es ->
    case world ^? worldEmployees . ix login of
        Just e  -> pure $ viewEmployeePage auth world e
        Nothing -> pure $ notFoundPage auth $
            "Cannot find user " <> loginToText login

listEmployeesPageImpl
    :: Ctx
    -> Maybe Login
    -> Handler (HtmlPage "list-employees")
listEmployeesPageImpl ctx mfu = withAuthUser ctx mfu $ \auth world _es ->
    pure $ listEmployeesPage auth world

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
    withAuthUser' forbiddenPage ctx mfu (\fu w es -> lift $ f fu w es)
