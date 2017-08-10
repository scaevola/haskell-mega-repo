{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE TypeOperators  #-}
module Futurice.App.FUM.API.Pages where

import Futurice.Prelude
import Prelude ()

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

type CreateEmployeePageEndpoint =
    "employees" :> "create" :>
    SSOUser :>
    Capture "personio-id" Personio.EmployeeId :>
    Get '[HTML] (HtmlPage "create-employee")

createEmployeePageEndpoint :: Proxy CreateEmployeePageEndpoint
createEmployeePageEndpoint = Proxy

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
