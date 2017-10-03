{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.FUM.Pages.CreateGroup (createGroupPage) where

import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.Command
import Futurice.App.FUM.Markup
import Futurice.App.FUM.Types

--import qualified Personio

createGroupPage
    :: AuthUser
    -> World                    -- ^ the world
    -> HtmlPage "create-group"
createGroupPage auth _world = fumPage_ "Create group" auth $ do
    -- Title
    fumHeader_ "Create group" []

    -- Form
    commandHtml' (Proxy :: Proxy CreateGroup) $ 
        vNothing :*
        vNothing :*
        vNothing :*
        Nil
