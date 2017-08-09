{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.FUM.Pages.ListEmployees (listEmployeesPage) where

import Control.Lens     (hasn't, forOf_)
import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.Markup
import Futurice.App.FUM.Types

listEmployeesPage
    :: AuthUser
    -> World     -- ^ the world
    -> HtmlPage "list-employees"
listEmployeesPage auth world = fumPage_ "Employees" auth $ do
    -- Title
    fumHeader_ "Employees" [] -- TODO: name

    when (hasn't (worldEmployees . folded) world) $
        row_ $ large_ 12 [ class_ "callout warning" ] $
            em_ "No employees"

    table_ $
        tbody_ $ forOf_ (worldEmployees . folded) world $ \e -> tr_ $ do
            td_ "yees"
            td_ $ toHtml $ show e
