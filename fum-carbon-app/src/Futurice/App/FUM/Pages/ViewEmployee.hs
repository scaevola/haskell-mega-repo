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
    let login = e ^. employeeLogin
    -- Title
    fumHeader_ "Employee" [Just $ loginToText login]

    todos_ [ "picture", "its editing" ]

    fullRow_ $ table_ $ tbody_ $ do
        vertRow_ "Name" $ toHtml $ e ^. employeeName
        vertRow_ "Login" $ toHtml login
        vertRow_ "Personio ID" $ toHtml $ e ^. employeePersonioId
        vertRow_ "Status" $ toHtml $ e ^. employeeStatus

        let mp = personio ^? ix (e ^. employeePersonioId)
        mcase mp (vertRow_ "Personio" $ em_ "cannot find id") $ \p -> do
            vertRow_ "Office"  $ toHtml $ p ^. Personio.employeeOffice
            vertRow_ "Tribe"   $ toHtml $ p ^. Personio.employeeTribe
            vertRow_ "Phone"   $ traverse_ toHtml $ p ^. Personio.employeeWorkPhone
            vertRow_ "Email"   $ traverse_ toHtml $ p ^. Personio.employeeEmail
            vertRow_ "Int/Ext" $ traverse_ toHtml $ p ^. Personio.employeeEmploymentType
            vertRow_ "GitHub"  $ traverse_ toHtml $ p ^. Personio.employeeGithub
            -- TODO: what else to show?
            --
    todos_ [ "show flowdock", "show contract span" ]

    block_ "Groups" $ do
        let groups = world ^.. worldEmployeeGroups . ix login . folded
        when (null groups) $
            row_ $ large_ 12 [ class_ "callout warning" ] $
                em_ "No groups"

        fullRow_ $ table_ $ do
            thead_ $ tr_ $ do
                th_ "Name"
                th_ "Type"
                th_ mempty

            tbody_ $ for_ groups $ \g -> tr_ $ do
                td_ $ a_ [ viewGroupHref_ $ g ^. groupName] $ toHtml $ g ^. groupName
                td_ $ toHtml $ g ^. groupType
                td_ "Remove TODO"

        subheader_ "Add to group"
        commandHtml' (Proxy :: Proxy AddEmployeeToGroup) $
            -- TODO: filter not editable groups
            vGroups (const True) world :*
            vHidden login :*
            Nil

        todos_ ["removal of groups"]

    block_ "Email addresses" $ do
        when (null $ e ^. employeeEmailAliases) $
            row_ $ large_ 12 [ class_ "callout warning" ] $
                em_ "No email addresses"

        fullRow_ $ table_ $ tbody_ $
            for_ (e ^.. employeeEmailAliases . folded) $ \email -> tr_ $ do
                td_ $ toHtml email
                td_ $ commandHtmlSubmit (Proxy :: Proxy RemoveEmailFromEmployee) "Remove" "alert" $
                    vHidden login :*
                    vHidden email :*
                    Nil

        subheader_ "Add email address"
        commandHtml' (Proxy :: Proxy AddEmailToEmployee) $
            vHidden login :*
            vNothing :*
            Nil

    block_ "SSH Keys" $ do
        todos_ [ "show", "management" ]

    block_ "Password" $ do
        fullRow_ $ do
            "Expires at "
            toHtml $ formatHumanHelsinkiTime $ e ^. employeePasswordExp

            "Change TODO"

        todos_ [ "management" ]
