{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.FUM.Pages.ViewEmployee (viewEmployeePage) where

import Control.Lens (to)
import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.Markup
import Futurice.App.FUM.Types

viewEmployeePage
    :: AuthUser
    -> World     -- ^ the world
    -> Employee  -- ^ employees
    -> HtmlPage "view-employee"
viewEmployeePage auth _world e = fumPage_ "Employee" auth $ do
    -- Title
    fumHeader_ "Employee" [e ^? employeeLogin . to loginToText ]

    fullRow_ "PICTURE TODO"

    fullRow_ $ table_ $ tbody_ $ do
        vertRow_ "Name" "TODO"
        vertRow_ "Login" $ toHtml $ e ^. employeeLogin
        vertRow_ "Personio ID" $ toHtml $ e ^. employeePersonioId
        vertRow_ "Status" $ toHtml $ e ^. employeeStatus

    fullRow_ "TODO: information from Personio"

    subheader_ "Email addresses"
    fullRow_ "TODO"
    fullRow_ "Add alias: TODO"

    subheader_ "SSH Keys"
    fullRow_ "TODO"

    subheader_ "Groups"
    fullRow_ "TODO"

    subheader_ "Password"
    fullRow_ $ do
        "Expires at "
        toHtml $ formatHumanHelsinkiTime $ e ^. employeePasswordExp

        "Change TODO"
