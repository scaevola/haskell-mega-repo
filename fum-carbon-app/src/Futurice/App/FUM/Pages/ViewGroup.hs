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
viewGroupPage auth _world g = fumPage_ "Group" auth $ do
    -- Title
    fumHeader_ "Group" [g ^? groupName . getter groupNameToText ]

    fullRow_ $ table_ $ tbody_ $ do
        vertRow_ "Name" $ toHtml $ g ^. groupName
        vertRow_ "Type" $ toHtml $ g ^. groupType
