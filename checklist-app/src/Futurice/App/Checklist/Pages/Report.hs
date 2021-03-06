{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.Report (reportPage) where

import Prelude ()
import Futurice.Prelude
import Futurice.Lucid.Foundation
import Web.HttpApiData           (toQueryParam)

import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types

reportPage
    :: World       -- ^ the world
    -> AuthUser    -- ^ logged in user
    -> Maybe (Identifier Checklist)
    -> Maybe Day
    -> Maybe Day
    -> HtmlPage "report"
reportPage world authUser mcid fday tday = checklistPage_ "Employees" authUser $ do
    let employees' = sortOn (view employeeStartingDay) $
            (world ^.. worldArchive . folded . _1)
            <> (world ^.. worldEmployees . folded)

    let employees = employees'
          & maybe id (\d -> filter $ \e -> d <= e ^. employeeStartingDay) fday
          & maybe id (\d -> filter $ \e -> e ^. employeeStartingDay <= d) tday
          & maybe id (\cid -> filter $ \e -> cid == e ^. employeeChecklist) mcid

    -- Title
    header "Report" []

    -- List filtering controls
    row_ $ form_ [ futuId_ "selector", action_ $ "/reports", method_ "get" ] $ do
        largemed_ 5 $ label_ $ do
            "Checklist"
            select_ [ name_ "checklist"] $ do
                option_ [ value_ "" ] $ "Show all"
                for_ (world ^.. worldLists . folded) $ \cl -> do
                    let cid = cl ^. identifier
                    optionSelected_ (Just cid == mcid)
                        [ value_ $ cid ^. getter identifierToText ]
                        $ cl ^. nameHtml
        largemed_ 3 $ label_ $ do
            "Starting after"
            input_ [ name_ "day-from", type_ "date", value_ $ maybe "" toQueryParam fday ]
        largemed_ 3 $ label_ $ do
            "Starting before"
            input_ [ name_ "day-to", type_ "date", value_ $ maybe "" toQueryParam tday ]
        largemed_ 1 $ label_ $ do
            toHtmlRaw ("&nbsp;" :: Text)
            button_ [ class_ "button" ] $ "Filter"

    -- The table
    row_ $ large_ 12 $ table_ $ do
        thead_ $ tr_ $ do
            th_ [title_ "Status"]                      "S"
            th_ [title_ "Office"]                      "Off"
            th_ [title_ "Name" ]                       "Name"
            th_ [title_ "Checklist"]                   "List"
            th_ [title_ "Due date"]                    "Due date"
            th_ [title_ "Tribe"]                       "Tribe"
            th_ [title_ "Email"]                       "Email"
            th_ [title_ "Confirmed - contract signed"] "Confirmed"
        tbody_ $ for_ employees $ \employee -> tr_ $ do
            td_ $ contractTypeHtml $ employee ^. employeeContractType
            td_ $ locationHtml (Nothing :: Maybe Checklist) $ employee ^. employeeOffice
            td_ $ employee ^. nameHtml
            td_ $ checklistNameHtml world Nothing (employee ^. employeeChecklist) False
            td_ $ toHtml $ show $ employee ^. employeeStartingDay
            td_ $ toHtml $ employee ^. employeeTribe
            td_ $ toHtml $ fromMaybe "" $ employee ^. employeeContactEmail
            td_ $ bool (pure ()) (toHtmlRaw ("&#8868;" :: Text)) $ employee ^. employeeConfirmed
