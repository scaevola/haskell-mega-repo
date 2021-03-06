{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.Archive (archivePage) where

import Prelude ()
import Futurice.Prelude
import Futurice.Lucid.Foundation
import Servant.Utils.Links (safeLink)

import Futurice.App.Checklist.API
import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types

archivePage
    :: World       -- ^ the world
    -> AuthUser    -- ^ logged in user
    -> HtmlPage "archive"
archivePage world authUser@(_, viewerRole) = checklistPage_ "Employees" authUser $ do
    let employees = sortOn (view $ _1 . employeeStartingDay) $ world ^.. worldArchive . folded

    -- Title
    header "Archive" []

    -- The table
    row_ $ large_ 12 $ table_ $ do
        thead_ $ tr_ $ do
            th_ [title_ "Status"]                      "S"
            th_ [title_ "Office"]                      "Office"
            th_ [title_ "Name" ]                       "Name"
            th_ [title_ "Checklist"]                   "List"
            th_ [title_ "Due date"]                    "Due date"
            th_ [title_ "Confirmed - contract signed"] "Confirmed"
            viewerItemsHeader viewerRole
            th_ [title_ "Task items todo/done"]        "Tasks"
            th_                                        "Audit"
        tbody_ $ for_ employees $ \(employee, TodoCounter (Counter i j) perRole ) -> tr_ $ do
            td_ $ contractTypeHtml $ employee ^. employeeContractType
            td_ $ locationHtml (Nothing :: Maybe Checklist) $ employee ^. employeeOffice
            td_ $ employee ^. nameHtml
            td_ $ checklistNameHtml world Nothing (employee ^. employeeChecklist) False
            td_ $ toHtml $ show $ employee ^. employeeStartingDay
            td_ $ bool (pure ()) (toHtmlRaw ("&#8868;" :: Text)) $ employee ^. employeeConfirmed
            case perRole ^. ix viewerRole of
                Counter a b -> do
                    td_ $ toHtml (show a) *> "/" *> toHtml (show b)
                    td_ $ toHtml (show i) *> "/" *> toHtml (show j)
            td_ $ a_ [ href_ $ linkToText $ safeLink checklistApi employeeAuditPageEndpoint (employee ^. identifier) ] "Audit log"
