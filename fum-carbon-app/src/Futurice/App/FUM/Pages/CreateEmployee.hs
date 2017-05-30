{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.FUM.Pages.CreateEmployee (createEmployeePage) where

import Futurice.IdMap   (IdMap)
import Futurice.Lomake
import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.Markup
import Futurice.App.FUM.Types  hiding (employeeId)

import qualified Data.Text as T
import qualified Personio

createEmployeePage
    :: World                    -- ^ the world
    -> IdMap Personio.Employee  -- ^ employees
    -> Personio.Employee
    -> HtmlPage "create-employee"
createEmployeePage _world _es e = fumPage_ "Create employee" () $ do
    -- Title
    fumHeader_ "Create employee" [] -- TODO: name

    row_ $ large_ 12 $ dl_ $ do
        dt_ "Name"
        dd_ $ toHtml $ e ^. Personio.employeeFirst <> " " <> e ^. Personio.employeeLast
        dt_ "Hiring date"
        dd_ $ maybe "-" (toHtml . show) $ e ^. Personio.employeeHireDate
        dt_ "Contract end date"
        dd_ $ maybe "-" (toHtml . show) $ e ^. Personio.employeeEndDate

    -- Form
    lomakeHtml lomakeData createEmployeeForm
  where
    pid = e ^. Personio.employeeId
    lomakeData :: NP I CEFFields
    lomakeData =
        I pid :*
        I loginSuggestion :*
        I emailSuggestion :*
        Nil

    -- TODO: check used
    loginSuggestion :: Text
    loginSuggestion = canonicalize $ mconcat
        [ T.take 1 $ e ^. Personio.employeeFirst
        , T.take 3 $ e ^. Personio.employeeLast
        ]

    emailSuggestion :: Text
    emailSuggestion = canonicalize (e ^. Personio.employeeFirst <> "." <> e ^. Personio.employeeLast)

data CreateEmployeeForm = CreateEmployeeForm
    { _cefPersonioId :: !Personio.EmployeeId
    , _cefLogin      :: !Login
    , _cefEmail      :: !Email
    }

type CEFFields = '[Personio.EmployeeId, Text, Text]

createEmployeeForm :: Lomake CEFFields '[] CreateEmployeeForm
createEmployeeForm = CreateEmployeeForm
    <<$>> hiddenField "Personio.EmployeeId" Personio._EmployeeId
    <<*>> textField "Login"
    <<*>> textField "Email"