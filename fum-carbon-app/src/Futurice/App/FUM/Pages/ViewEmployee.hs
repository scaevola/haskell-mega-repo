{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.FUM.Pages.ViewEmployee (viewEmployeePage) where

import Futurice.Prelude
import Futurice.IdMap (IdMap)
import Prelude ()

import Futurice.App.FUM.Markup
import Futurice.App.FUM.Types

import qualified Personio

viewEmployeePage
    :: AuthUser
    -> World     -- ^ the world
    -> IdMap Personio.Employee
    -> Employee  -- ^ employees
    -> HtmlPage "view-employee"
viewEmployeePage auth _world personio e = fumPage_ "Employee" auth $ do
    -- Title
    fumHeader_ "Employee" [e ^? employeeLogin . getter loginToText ]

    fullRow_ "PICTURE TODO"

    fullRow_ $ table_ $ tbody_ $ do
        vertRow_ "Name" $ toHtml $ e ^. employeeName
        vertRow_ "Login" $ toHtml $ e ^. employeeLogin
        vertRow_ "Personio ID" $ toHtml $ e ^. employeePersonioId
        vertRow_ "Status" $ toHtml $ e ^. employeeStatus

        let mp = personio ^? ix (e ^. employeePersonioId)
        mcase mp (vertRow_ "Personio" $ em_ "cannot find id") $ \p -> do
            vertRow_ "Office" $ toHtml $ p ^. Personio.employeeOffice
            vertRow_ "Tribe" $ toHtml $ p ^. Personio.employeeTribe
            vertRow_ "Phone" $ traverse_ toHtml $ p ^. Personio.employeeWorkPhone
            -- TODO: what else to show?

    fullRow_ "TODO: information from Personio"

    subheader_ "Email addresses"

    when (null $ e ^. employeeEmailAliases) $
        row_ $ large_ 12 [ class_ "callout warning" ] $
            em_ "No email addresses"

    fullRow_ $ table_ $ tbody_ $
        for_ (e ^.. employeeEmailAliases . folded) $ \email -> tr_ $ do
            td_ $ toHtml email
            td_ $ button_ [ class_ "button" ] "Remove"

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
