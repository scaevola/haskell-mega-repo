{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.FUM.Pages.ViewGroup (viewGroupPage) where

import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.Markup
import Futurice.App.FUM.Types

viewGroupPage
    :: AuthUser
    -> World  -- ^ the world
    -> Group  -- ^ group
    -> HtmlPage "view-group"
viewGroupPage auth world g = fumPage_ "Group" auth $ do
    -- Title
    fumHeader_ "Group" [g ^? groupName . getter groupNameToText ]

    fullRow_ $ table_ $ tbody_ $ do
        vertRow_ "Name" $ toHtml $ g ^. groupName
        vertRow_ "Type" $ toHtml $ g ^. groupType

    subheader_ "Add member"
    fullRow_ "TODO"

    subheader_ "Members"
    fullRow_ $ table_ $ do
        thead_ $ tr_ $ do
            th_ "Login"
            th_ "Name"
            th_ "Status"

        tbody_ $ for_ (g ^.. groupEmployees . folded) $ \login -> tr_ $
            for_ (world ^? worldEmployees . ix login) $ \e -> do
                td_ $ loginToHtml $ e ^. employeeLogin
                td_ $ toHtml $ e ^. employeeName
                td_ $ toHtml $ e ^. employeeStatus
