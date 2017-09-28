{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Futurice.App.PlanMillSync.IndexPage (indexPage) where

import Control.Lens                (iforOf_)
import Data.Map.Lens               (toMapOf)
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Prelude ()
import Data.These (_These)
import Text.Regex.Applicative.Text (match)

import qualified FUM.Types.Login as FUM
import qualified Personio        as P
import qualified PlanMill        as PM

indexPage
    :: UTCTime
    -> [PM.User]
    -> [P.Employee]
    -> HtmlPage "index"
indexPage _now planmills personios = page_ "PlanMill sync" $ do
    fullRow_ $ h1_ "Personio ⇒ PlanMill sync"

    fullRow_ $ h2_ "Cross-check of people in PlanMill and Personio"
    fullRow_ $ table_ $ do
        thead_ $ tr_ $ do
            td_ "Login"
            td_ "Name"
            td_ "Tribe"
            td_ "Office"
            td_ "Contract type"
            td_ "Contract start"
            td_ "Contract end"
            
            td_ "PM Superior"
            td_ "PM team"
    
        tbody_ $ iforOf_ (ifolded . _These) employees $ \login (pm, p) -> tr_ $ do
            td_ $ toHtml login
            td_ $ toHtml $ p ^. P.employeeFullname
            td_ $ toHtml $ p ^. P.employeeTribe
            td_ $ toHtml $ p ^. P.employeeOffice
            td_ $ traverse_ (toHtml . show) $ p ^. P.employeeContractType
            td_ $ traverse_ (toHtml . show) $ p ^. P.employeeHireDate
            td_ $ traverse_ (toHtml . show) $ p ^. P.employeeEndDate

            td_ $ for_ (PM.uSuperior pm) $ \sv -> do
                "XXX"
                toHtml (show sv)
            td_ $ (toHtml . show) $ PM.uTeam pm  
  where
    planmillMap :: Map FUM.Login PM.User
    planmillMap = toMapOf (folded . getter f . _Just . ifolded) planmills
      where
        f u = do
            login <- match loginRe (PM.uUserName u)
            pure (login, u)

        loginRe = "https://login.futurice.com/openid/" *> FUM.loginRegexp

    personioMap :: Map FUM.Login P.Employee
    personioMap = toMapOf (folded . getter f . _Just . ifolded) personios
      where
        f u = do
            login <- u ^. P.employeeLogin
            pure (login, u)

    employees :: Map FUM.Login (These PM.User P.Employee)
    employees = align planmillMap personioMap

{-

    fullRow_ $ h2_ "Only in PlanMill, not in Personio"
    fullRow_ $ i_ "People in PlanMill organisation, not mentioned in Personio"
    fullRow_ $ do
        table_ $ do
            thead_ $ tr_ $ do
                td_ mempty
                td_ "Username"
                td_ "Real name"
                td_ $ "Personio" >> sup_ "?"
                td_ $ "FUM" >> sup_ "?"
                td_ $ "Contract end date"  >> sup_ "?"
            tbody_ $ for_ githubs $ \u -> do
                let login = GH.userLogin u
                unless (personioLogins ^. contains login) $ tr_ $ do
                    td_ $ checkbox_ False []
                    td_ $ toHtml $ GH.userLogin u
                    td_ $ maybe "" toHtml $ GH.userName u

                    case personioMap ^? ix login of
                        Nothing -> td_ mempty >> td_ mempty >> td_ mempty
                        Just e -> do
                            td_ $ toHtml $ e ^. P.employeeId
                            td_ $ traverse_ toHtml $ e ^. P.employeeLogin
                            td_ $ traverse_ (toHtml . show) $ e ^. P.employeeEndDate

        div_ [ class_ "button-group" ] $
            button_ [ class_ "button alert"] "Remove"


    fullRow_ $ h2_ "Not in PlanMill, only in Personio"
    fullRow_ $ i_ "People with PlanMill information in Personio, but not added to PlanMill"

    fullRow_ $ do
        table_ $ do
            thead_ $ tr_ $ do
                td_ mempty
                td_ "Personio"
                td_ "Name"
                td_ "PlanMill"
            tbody_ $ for_ personios $ \e ->
                for_ (e ^. P.employeeGithub) $ \glogin ->
                    when (P.employeeIsActive now e && not (githubLogins ^. contains glogin)) $ tr_ $ do
                        td_ $ checkbox_ False []
                        td_ $ toHtml $ e ^. P.employeeId
                        td_ $ toHtml $ e ^. P.employeeFullname
                        td_ $ toHtml glogin

        div_ [ class_ "button-group" ] $
            button_ [ class_ "button warning"] "Add"
  where
    githubLogins :: Set (GH.Name GH.User)
    githubLogins = setOf (folded . getter GH.userLogin) githubs

    personioLogins :: Set (GH.Name GH.User)
    personioLogins = setOf (folded . filtered (P.employeeIsActive now) . P.employeeGithub . _Just) personios
        -- add pinned users to personio set, so we don't remove them
        <> setOf folded pinned

    personioMap :: Map (GH.Name GH.User) P.Employee
    personioMap = toMapOf (folded . getter f . _Just . ifolded) personios
      where
        f e = (,e) <$> e ^. P.employeeGithub
-}
