{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.Personio (personioPage) where

import Data.Maybe                (isJust)
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Prelude ()

import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types

import qualified Personio

personioPage
    :: World       -- ^ the world
    -> AuthUser    -- ^ logged in user
    -> UTCTime     -- ^ now
    -> [Personio.Employee]
    -> HtmlPage "personio"
personioPage world authUser now employees0 = checklistPage_ "Import from personio" authUser $ do
    -- Title
    header "Import from Personio" []

    -- Table
    row_ $ large_ 12 $ table_ $ do
        thead_ $ tr_ $ do
            th_ "Name"
            th_ "Exists"
            th_ "Login"
            th_ "Tribe"
            th_ "Office"
            th_ "Hire date"
            th_ "Create"

        tbody_ $ for_ employees $ \e -> tr_ $ do
            td_ $ toHtml $ (e ^. Personio.employeeFirst) <> " " <> (e ^. Personio.employeeLast)
            td_ $ boolHtml $ any
                (== e ^. Personio.employeeLogin)
                $ world ^.. worldEmployees . folded .  employeeFUMLogin
            td_ $ traverse_ toHtml $ e ^. Personio.employeeLogin
            td_ $ toHtml $  e ^. Personio.employeeTribe
            td_ $ toHtml $  e ^. Personio.employeeOffice
            td_ $ traverse_ (toHtml . show) $ e ^. Personio.employeeHireDate
            td_ $ button_ [ class_ "button" ] "TODO"

  where
    employees = employees0
        & filter (\e -> maybe False (utctDay now <) $ e ^. Personio.employeeHireDate)
        & filter (\e -> isJust (e ^. Personio.employeeLogin))
        & sortOn (view Personio.employeeHireDate)

    boolHtml :: Monad m => Bool -> HtmlT m ()
    boolHtml True = "YES"
    boolHtml False = "NO"
