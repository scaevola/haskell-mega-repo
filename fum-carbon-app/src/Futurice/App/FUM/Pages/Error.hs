{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Futurice.App.FUM.Pages.Error where

import Prelude ()
import Futurice.Prelude

import Futurice.App.FUM.Markup
import Futurice.App.FUM.Types

notFoundPage :: AuthUser -> Text -> HtmlPage sym
notFoundPage auth msg = fumPage_ "Not found" auth $
    row_ $ large_ 12 [ class_ "callout alert" ] $
        toHtml msg

-- TODO: Do proper
forbiddenPage :: HtmlPage sym
forbiddenPage = fumPage_ "Forbidden" ($(mkLogin "guest"), RightsOther) $
    row_ $ large_ 12 [ class_ "callout alert" ] "Forbidden page"
