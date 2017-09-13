{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.FUM.Pages.ViewEmployee (viewEmployeePage) where

import Futurice.IdMap   (IdMap)
import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.Command
import Futurice.App.FUM.Lomake
import Futurice.App.FUM.Markup
import Futurice.App.FUM.Types

import qualified Personio

viewEmployeePage
    :: AuthUser
    -> World     -- ^ the world
    -> IdMap Personio.Employee
    -> Employee  -- ^ employees
    -> HtmlPage "view-employee"
viewEmployeePage auth world personio e = fumPage_ "Employee" auth $ do
    -- Title
    fumHeader_ "Employee" [e ^? employeeLogin . getter loginToText ]

    todos_ [ "picture", "its editing" ]

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
            --
    todos_ [ "show github", "show flowdock", "show internal/external", "show contractEndDate" ]

    block_ "Email addresses" $ do
        when (null $ e ^. employeeEmailAliases) $
            row_ $ large_ 12 [ class_ "callout warning" ] $
                em_ "No email addresses"

        fullRow_ $ table_ $ tbody_ $
            for_ (e ^.. employeeEmailAliases . folded) $ \email -> tr_ $ do
                td_ $ toHtml email
                td_ $ button_ [ class_ "button" ] "Remove"

        todos_ [ "Remove doesn't work", "Add alias" ]

    block_ "SSH Keys" $ do
        todos_ [ "show", "management" ]

    block_ "Groups" $ do
        subheader_ "Add to group"
        commandHtml' (Proxy :: Proxy AddEmployeeToGroup) $
            -- TODO: filter not editable groups
            vGroups (const True) world :*
            vHidden (e ^. employeeLogin) :*
            Nil

        todos_ ["Show groups", "removal of groups"]

    block_ "Password" $ do
        fullRow_ $ do
            "Expires at "
            toHtml $ formatHumanHelsinkiTime $ e ^. employeePasswordExp

            "Change TODO"

        todos_ [ "management" ]
