{-# LANGUAGE OverloadedStrings #-}
-- | Safe links for Pages API
module Futurice.App.FUM.Pages.Href (
    module Futurice.App.FUM.Pages.Href,
    ) where

import Futurice.Lucid.Foundation
import Futurice.Prelude
import Prelude ()
import Servant.Utils.Links       (Link, safeLink)
import Web.HttpApiData           (toUrlPiece)
import FUM.Types.Login           (Login)

import Futurice.App.FUM.API.Pages

import qualified Personio

-------------------------------------------------------------------------------
-- Index page
-------------------------------------------------------------------------------

indexPageHref_ :: Attribute
indexPageHref_ = href_ $ linkToText $ safeLink fumCarbonPagesApi
    indexPageEndpoint

-------------------------------------------------------------------------------
-- Employees
-------------------------------------------------------------------------------

listEmployeesHref_ :: Attribute
listEmployeesHref_ = href_ $ linkToText $ safeLink fumCarbonPagesApi
    listEmployeesPageEndpoint

createEmployeeHref_ :: Personio.EmployeeId -> Attribute
createEmployeeHref_ eid = href_ $ linkToText $ safeLink fumCarbonPagesApi
    createEmployeePageEndpoint
    eid

viewEmployeeHref_ :: Login -> Attribute
viewEmployeeHref_ = href_ . viewEmployeeHrefText

viewEmployeeHrefText :: Login -> Text
viewEmployeeHrefText login = linkToText $ safeLink fumCarbonPagesApi
    viewEmployeePageEndpoint
    login

-------------------------------------------------------------------------------
-- Internals
-------------------------------------------------------------------------------

linkToText :: Link -> Text
linkToText l = "/" <> toUrlPiece l

linkToAttribute :: Link -> Attribute
linkToAttribute = href_ . linkToText
