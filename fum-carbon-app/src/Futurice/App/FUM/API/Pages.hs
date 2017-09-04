{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Futurice.App.FUM.API.Pages where

import Futurice.Prelude
import Prelude ()

import FUM.Types.GroupName       (GroupName)
import FUM.Types.Login           (Login)
import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Servant          (SSOUser)
import Servant.API
import Servant.HTML.Lucid        (HTML)

import qualified Personio

type FumCarbonPagesApi = IndexPageEndpoint
    -- Employees
    :<|> ListEmployeesPageEndpoint
    :<|> ViewEmployeePageEndpoint
    :<|> CreateEmployeePageEndpoint
    -- Groups
    :<|> ListGroupsPageEndpoint
    :<|> ViewGroupPageEndpoint
    :<|> CreateGroupPageEndpoint

fumCarbonPagesApi :: Proxy FumCarbonPagesApi
fumCarbonPagesApi = Proxy

-------------------------------------------------------------------------------
-- Index
-------------------------------------------------------------------------------

type IndexPageEndpoint =
    SSOUser :>
    Get '[HTML] (HtmlPage "indexpage")

indexPageEndpoint :: Proxy IndexPageEndpoint
indexPageEndpoint = Proxy

-------------------------------------------------------------------------------
-- Employee
-------------------------------------------------------------------------------

type ListEmployeesPageEndpoint =
    "employees" :>
    SSOUser :>
    Get '[HTML] (HtmlPage "list-employees")

listEmployeesPageEndpoint :: Proxy ListEmployeesPageEndpoint
listEmployeesPageEndpoint = Proxy

type ViewEmployeePageEndpoint =
    "employee" :>
    SSOUser :>
    Capture "login" Login :>
    Get '[HTML] (HtmlPage "view-employee")

viewEmployeePageEndpoint :: Proxy ViewEmployeePageEndpoint
viewEmployeePageEndpoint = Proxy

type CreateEmployeePageEndpoint =
    "employees" :> "create" :>
    SSOUser :>
    Capture "personio-id" Personio.EmployeeId :>
    Get '[HTML] (HtmlPage "create-employee")

createEmployeePageEndpoint :: Proxy CreateEmployeePageEndpoint
createEmployeePageEndpoint = Proxy

-------------------------------------------------------------------------------
-- Groups
-------------------------------------------------------------------------------

type ListGroupsPageEndpoint =
    "groups" :>
    SSOUser :>
    Get '[HTML] (HtmlPage "list-groups")

listGroupsPageEndpoint :: Proxy ListGroupsPageEndpoint
listGroupsPageEndpoint = Proxy

type ViewGroupPageEndpoint =
    "group" :>
    SSOUser :>
    Capture "group-name" GroupName :>
    Get '[HTML] (HtmlPage "view-group")

viewGroupPageEndpoint :: Proxy ViewGroupPageEndpoint
viewGroupPageEndpoint = Proxy

type CreateGroupPageEndpoint =
    "groups" :> "create" :>
    SSOUser :>
    Get '[HTML] (HtmlPage "create-group")

createGroupPageEndpoint :: Proxy CreateGroupPageEndpoint
createGroupPageEndpoint = Proxy
