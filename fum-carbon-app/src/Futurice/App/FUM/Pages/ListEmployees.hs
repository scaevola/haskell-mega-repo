{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.FUM.Pages.ListEmployees (listEmployeesPage) where

import Control.Lens     (Getting, hasn't)
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
    fumHeader_ "Employees" []

    when (hasITRights auth) $ fullRow_ $ 
        futuLinkButton_ fromPersonioPageHref_ "Create employee"

    when (hasn't (worldEmployees . folded) world) $
        row_ $ large_ 12 [ class_ "callout warning" ] $
            em_ "No employees"

    fullRow_ $ table_ $ do
        thead_ $ tr_ $ do
            th_ "Login"
            th_ "Name"
            th_ "Personio ID"
            th_ "Status"

        tbody_ $ forSortedOnOf_ (view employeeLogin) (worldEmployees . folded) world $ \e -> tr_ $ do
            td_ $ loginToHtml $ e ^. employeeLogin
            td_ $ toHtml $ e ^. employeeName
            td_ $ toHtml $ e ^. employeePersonioId
            td_ $ toHtml $ e ^. employeeStatus

-- | This isn't super effective, yet good enough.
forSortedOnOf_
    :: (Applicative f, Ord b)
    => (a -> b)
    -> Getting (Endo [a]) s a
    -> s
    -> (a -> f ())
    -> f ()
forSortedOnOf_ metric l s = for_ (sortOn metric $ s ^.. l)
