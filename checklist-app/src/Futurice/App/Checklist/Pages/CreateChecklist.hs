{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.CreateChecklist (createChecklistPage) where

import Prelude ()
import Futurice.Prelude
import Futurice.Lucid.Foundation

import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types

createChecklistPage
    :: World
    -> AuthUser    -- ^ logged in user
    -> HtmlPage "create-checklist"
createChecklistPage _world authUser = checklistPage_ ("Create checklist") authUser $ do
    -- Title
    header "Create checklist" []

    -- Edit
    row_ [ id_ "futu-checklist-new" ] $ large_ 12 $ do
        row_ $ large_ 12 $
            label_ $ do
                "Name"
                -- TODO: change id to futu-id
                input_ [ id_ "futu-checklist-name", type_ "text" ]

        row_ $ large_ 12 $ div_ [ class_ "button-group" ] $ do
            button_ [ class_ "button success", data_ "futu-action" "submit" ] $ "Create"
            button_ [ class_ "button", data_ "futu-action" "reset" ] $ "Reset"
