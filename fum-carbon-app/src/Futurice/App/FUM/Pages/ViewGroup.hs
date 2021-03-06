{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.FUM.Pages.ViewGroup (viewGroupPage) where

import Algebra.Lattice  (bottom)
import Control.Lens     (contains)
import Futurice.IdMap   (IdMap)
import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.ACL
import Futurice.App.FUM.Command
import Futurice.App.FUM.Lomake
import Futurice.App.FUM.Markup
import Futurice.App.FUM.Types

import qualified Personio

viewGroupPage
    :: AuthUser
    -> World                   -- ^ the world
    -> IdMap Personio.Employee -- ^ personio data
    -> Group                   -- ^ group
    -> HtmlPage "view-group"
viewGroupPage auth world personio g = fumPage_ "Group" auth $ do
    -- Title
    fumHeader_ "Group" [g ^? groupName . getter groupNameToText ]

    fullRow_ $ table_ $ tbody_ $ do
        vertRow_ "Name" $ toHtml $ g ^. groupName
        vertRow_ "Type" $ toHtml $ g ^. groupType
        unless (null (g ^. groupEditor)) $
            vertRow_ "Editor groups" $ forWith_ ", " (g ^. groupEditor) toHtml
        unless (bottom == g ^. groupMatch) $
            vertRow_ "Automatic includes" $ toHtml $ g ^. groupMatch

    todos_ ["edit group type, remove group, case-insenstive names"]

    when (canEditGroup (authLogin auth) (g ^. groupName) world) $ do
        block_ "Add member" $ commandHtmlSubmit (Proxy :: Proxy AddEmployeeToGroup) "Add member" "success" $
            vHidden (g ^. groupName) :*
            vEmployees (\e -> notElem e $ g ^.. groupEmployees . folded) world :*
            Nil

    block_ "Members" $ do
        fullRow_ $ table_ $ do
            thead_ $ tr_ $ do
                th_ "Login"
                th_ "Name"
                th_ "Status"
                th_ mempty

            tbody_ $ for_ (g ^.. groupEmployees . folded) $ \login -> tr_ $
                for_ (world ^? worldEmployees . ix login) $ \e -> do
                    td_ $ loginToHtml $ e ^. employeeLogin
                    td_ $ toHtml $ e ^. employeeName
                    td_ $ toHtml $ e ^. employeeStatus
                    td_ $ commandHtmlSubmit (Proxy :: Proxy RemoveEmployeeFromGroup) "Remove" "warning" $
                        vHidden (g ^. groupName) :*
                        vHidden login :*
                        Nil

    unless (bottom == g ^. groupMatch) $ block_ "Automatic members" $ do
        fullRow_ $ table_ $ do
            thead_ $ tr_ $ do
                th_ "Login"
                th_ "Name"
                th_ "Status"

            tbody_ $ for_ (world ^.. worldEmployees . folded) $ \e ->
                for_ (personio ^? ix (e ^. employeePersonioId)) $ \p -> do
                    let pe = p ^. Personio.employeeEmploymentType
                    let po = p ^. Personio.employeeOffice
                    let pt = p ^. Personio.employeeTribe
                    when (groupMatchToPredicate (g ^. groupMatch) pe po pt) $ tr_ $ do
                        td_ $ loginToHtml $ e ^. employeeLogin
                        td_ $ toHtml $ e ^. employeeName
                        td_ $ toHtml $ e ^. employeeStatus

    when (authRights auth == RightsIT) $ do
        block_ "Editor groups" $ fullRow_ $ do
            commandHtmlSubmit (Proxy :: Proxy AddEditorGroup) "Add editor group" "success" $
                vHidden (g ^. groupName) :*
                vGroups (\x -> not (g ^. groupEditor . contains x)) world :*
                Nil

            unless (null $ g ^. groupEditor) $ table_ $ do
                thead_ $ do
                    th_ "Name"
                    th_ mempty

                tbody_ $ for_ (g ^. groupEditor) $ \editorName -> do
                    td_ $ toHtml editorName
                    td_ $ commandHtmlSubmit (Proxy :: Proxy RemoveEditorGroup) "Remove" "warning" $
                        vHidden (g ^. groupName) :*
                        vHidden editorName :*
                        Nil

        block_ "Automatic membership" $ fullRow_ $
            commandHtmlSubmit (Proxy :: Proxy ChangeGroupMatch) "Change group match" "success" $
                vHidden (g ^. groupName) :*
                vJust (g ^. groupMatch) :*
                Nil
