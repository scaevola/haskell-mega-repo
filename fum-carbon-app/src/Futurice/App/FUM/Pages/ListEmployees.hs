{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.FUM.Pages.ListEmployees (listEmployeesPage) where

import Control.Lens     (Getting, LensLike', forOf_, hasn't)
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

    fullRow_ $ table_ $ do
        thead_ $ tr_ $ do
            th_ "Login"
            th_ "Name"
            th_ "Personio ID"
            th_ "Status"

        tbody_ $ forOf_ (sortedOnOf (view employeeLogin) $ worldEmployees . folded) world $ \e -> tr_ $ do
            -- TODO: name
            td_ $ loginToHtml $ e ^. employeeLogin
            td_ $ toHtml $ e ^. employeeName
            td_ $ toHtml $ e ^. employeePersonioId
            td_ $ toHtml $ e ^. employeeStatus

-- | This isn't super effective, yet good enough.
sortedOnOf
    :: (Ord b, Contravariant f, Applicative f)
    => (a -> b)
    -> Getting (Endo [a]) s a
    -> LensLike' f s a
sortedOnOf m l f s = phantom $ traverse_ f $ sortOn m $ s ^.. l
