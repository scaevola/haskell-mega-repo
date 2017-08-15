{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.FUM.Pages.CreateEmployee (createEmployeePage) where

import Futurice.IdMap   (IdMap)
import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.Command
import Futurice.App.FUM.Markup
import Futurice.App.FUM.Types

import qualified Personio

createEmployeePage
    :: AuthUser
    -> World                    -- ^ the world
    -> IdMap Personio.Employee  -- ^ employees
    -> Personio.Employee
    -> HtmlPage "create-employee"
createEmployeePage auth _world _es e = fumPage_ "Create employee" auth $ do
    -- Title
    fumHeader_ "Create employee" [] -- TODO: name

    row_ $ large_ 12 $ table_ $ tbody_ $ do
        vertRow_ "Name" $
            toHtml $ e ^. Personio.employeeFirst <> " " <> e ^. Personio.employeeLast
        vertRow_ "Personio id" $
            toHtml $ e ^. Personio.employeeId
        vertRow_ "Login" $
            traverse_ toHtml $ e ^. Personio.employeeLogin
        vertRow_ "Email" $
            toHtml $ e ^. Personio.employeeEmail
        vertRow_ "Hiring date" $
            maybe "-" (toHtml . show) $ e ^. Personio.employeeHireDate
        vertRow_ "Contract end date" $
            maybe "-" (toHtml . show) $ e ^. Personio.employeeEndDate

    -- Form
    commandHtml' (Proxy :: Proxy CreateEmployee) $ 
        Just (e ^. Personio.employeeId) :*
        (e ^. Personio.employeeLogin) :*
        Nothing :*
        (e ^? Personio.employeeFullname) :*
        (e ^? Personio.employeeEmail) :*
        Nil
