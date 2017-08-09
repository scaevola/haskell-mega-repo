{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.FUM.Pages.ViewEmployee (viewEmployeePage) where

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
    fumHeader_ "Employee" [] -- TODO: name

    toHtml $ show e
