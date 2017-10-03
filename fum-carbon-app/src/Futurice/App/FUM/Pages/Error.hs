{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.FUM.Pages.Error where

import Control.Lens     (filtered)
import Futurice.IdMap   (IdMap)
import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.Command
import Futurice.App.FUM.Markup
import Futurice.App.FUM.Types

import qualified Personio

notFoundPage :: AuthUser -> Text -> HtmlPage sym
notFoundPage auth msg = fumPage_ "Not found" auth $
    row_ $ large_ 12 [ class_ "callout alert" ] $
        toHtml msg

forbiddenPage :: Maybe (Login, World, IdMap Personio.Employee) -> HtmlPage sym
forbiddenPage tr = fumPage_ "Forbidden" (AuthUser $(mkLogin "guest") RightsOther) $ do
    row_ $ large_ 12 [ class_ "callout alert" ] "Forbidden page"

    for_ tr  $ \(login, world, es) ->
        when (null $ world ^. worldEmployees) $ do
            subheader_ "Bootstrap"

            for_ (es ^? folded . filtered (\e -> e ^. Personio.employeeLogin == Just login)) $ \e -> do
                row_ $ large_ 12 $ table_ $ tbody_ $ do
                    vertRow_ "Name" $
                        toHtml $ e ^. Personio.employeeFirst <> " " <> e ^. Personio.employeeLast
                    vertRow_ "Personio id" $
                        toHtml $ e ^. Personio.employeeId
                    vertRow_ "Login" $
                        traverse_ toHtml $ e ^. Personio.employeeLogin
                    vertRow_ "Email" $
                        toHtml $ e ^. Personio.employeeEmail
                    vertRow_ "Hiring date" $
                        maybe "-" (toHtml . show) $ e ^. Personio.employeeHireDate
                    vertRow_ "Contract end date" $
                        maybe "-" (toHtml . show) $ e ^. Personio.employeeEndDate

                commandHtml' (Proxy :: Proxy Bootstrap) $
                    vJust (e ^. Personio.employeeId) :*
                    V (e ^. Personio.employeeLogin) [] :*
                    vNothing :*
                    V (e ^? Personio.employeeFullname) [] :*
                    V (e ^? Personio.employeeEmail) [] :*
                    vNothing :*
                    vNothing :*
                    Nil
