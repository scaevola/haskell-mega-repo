{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Futurice.App.PlanMillSync.IndexPage (indexPage) where

import Control.Lens                (IndexedGetting, ifoldMapOf, (.=))
import Control.Monad.State.Strict  (State, runState)
import Data.Char                   (isDigit)
import Data.Map.Lens               (toMapOf)
import Data.Maybe                  (isNothing)
import Data.These                  (_These)
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Prelude ()
import Text.Regex.Applicative.Text (RE', anySym, match, psym)

import qualified Data.Text                     as T
import qualified FUM.Types.Login               as FUM
import qualified Personio                      as P
import qualified PlanMill                      as PM
import qualified Text.Regex.Applicative.Common as RE

import Futurice.App.PlanMillSync.Types (PMUser (..))

itoListWithOf :: IndexedGetting i (Endo [x]) s a -> (i -> a -> x) ->  s -> [x]
itoListWithOf l f s = appEndo (ifoldMapOf l (\i a -> Endo (f i a :)) s) []

indexPage
    :: UTCTime
    -> [PMUser]
    -> [P.Employee]
    -> HtmlPage "index"
indexPage now planmills personios = page_ "PlanMill sync" $ do
    fullRow_ $ h1_ "Personio ⇒ PlanMill sync"

    fullRow_ $ h2_ "Cross-check of people in PlanMill and Personio"
    fullRow_ $ div_ [ class_ "callout alert "] $ ul_ $ do
        li_ $ "PlanMill data updates at night, so it can be out-of-date if there are recent changes."
        li_ $ do 
            "When values are differeent, both are shown: "
            b_ "Personio ≠ PlanMill"
            "."

    fullRow_ $ table_ $ do
        thead_ $ tr_ $ do
            td_ "Login"
            td_ "Personio"
            td_ "Planmill"
            td_ "Name"
            td_ "Tribe"
            td_ "Office"
            td_ "Contract type"
            td_ "Contract span"

            td_ "PM Superior"
            td_ "Cost center = PM Team"

        tbody_ $ do
            let elements0 = itoListWithOf (ifolded . _These) processBoth employees
            let elements1 = map (\h -> runState (commuteHtmlT h) True) elements0
            let elements2 = map fst $ sortOn snd elements1
            traverse_ id elements2
  where
    processBoth :: MonadState Bool m => FUM.Login -> (PMUser, P.Employee) -> HtmlT m ()
    processBoth login (pm, p) = tr_ $ do
        let pmu = pmUser pm
        let pmt = pmTeam pm

        td_ $ toHtml login
        td_ $ toHtml $ p ^. P.employeeId
        td_ $ toHtml $ pmu ^. PM.identifier
        td_ $ toHtml $ p ^. P.employeeFullname
        td_ $ toHtml $ p ^. P.employeeTribe
        td_ $ toHtml $ p ^. P.employeeOffice

        -- Contract type
        cell_ $ case p ^. P.employeeContractType of
            Nothing -> markErrorCell
            Just pContract -> do
                toHtml (show pContract)
                unless (contractTypeOk pContract (pmContract pm)) $ do
                    markErrorCell
                    " ≠ "
                    toHtml (pmContract pm)

        -- Contract span
        cell_ $ do
            let ndash = "–"
            let arrow = " →" -- space is intentional

            let pStart = p ^. P.employeeHireDate
            let pEnd = p ^. P.employeeEndDate

            let pmStart = PM.uHireDate pmu
            let pmEnd = PM.uDepartDate pmu

            -- there should be starting date(s)!
            when (isNothing pStart) markErrorCell
            when (isNothing pmStart) markErrorCell

            when (p ^. P.employeeContractType == Just P.FixedTerm) $ do
                pure ()
                -- when (isNothing pEnd) markErrorCell
                -- when (isNothing pmEnd) markErrorCell

            let formatDateSpan s e =
                    let s' = maybe "?" show s
                        e' = maybe arrow (\x -> ndash ++ show x) e
                    in s' ++ e'

            toHtml $ formatDateSpan pStart pEnd

            let startDifferent = case (pStart, pmStart) of
                    (Nothing, Nothing) -> False
                    (Nothing, Just _)  -> True
                    (Just _,  Nothing) -> True
                    -- planmill start date could be larger than actual start date.
                    (Just a, Just b)   -> a > b

            -- TODO: should be just /=
            let endDifferent = case (pEnd, pmEnd) of
                    (Nothing, Nothing) -> False
                    (Nothing, Just _)  -> False
                    (Just a,  Nothing) -> a > utctDay now -- TODO?
                    (Just a, Just b)   -> a /= b

            when (endDifferent || startDifferent) $ do
                markErrorCell
                " ≠ "
                toHtml $ formatDateSpan pmStart pmEnd

        -- superior
        cell_ $ for_ (PM.uSuperior pmu) $ \sv -> do
            markErrorCell
            toHtml (show sv)

        -- Cost centers
        cell_ $ do
            let planmillCC = match prefixNumber =<< (PM.tName <$> pmt)
            let personioCC = match prefixNumber =<< (p ^. P.employeeCostCenter)
            let ccEqual = planmillCC == personioCC
            traverse_ toHtml $  p ^. P.employeeCostCenter
            unless ccEqual $ do
                markErrorCell
                " ≠ "
                traverse_ (toHtml . PM.tName) pmt


    planmillMap :: Map FUM.Login PMUser
    planmillMap = toMapOf (folded . getter f . _Just . ifolded) planmills
      where
        f u = do
            login <- match loginRe (PM.uUserName (pmUser u))
            pure (login, u)

        loginRe = "https://login.futurice.com/openid/" *> FUM.loginRegexp

    personioMap :: Map FUM.Login P.Employee
    personioMap = toMapOf (folded . getter f . _Just . ifolded) personios
      where
        f u = do
            login <- u ^. P.employeeLogin
            pure (login, u)

    employees :: Map FUM.Login (These PMUser P.Employee)
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

-------------------------------------------------------------------------------
-- Contract type
-------------------------------------------------------------------------------

contractTypeOk :: P.ContractType -> Text -> Bool
contractTypeOk P.Permanent      t               = "Permanent" `T.isInfixOf` t
-- All-in people don't have working time
contractTypeOk P.PermanentAllIn t               = "no working time" `T.isInfixOf` t
-- fixed terms are either subcontractors or permanent employees
contractTypeOk P.FixedTerm      "Subcontractor" = True
contractTypeOk P.FixedTerm      t               = "Permanent" `T.isInfixOf` t

-------------------------------------------------------------------------------
-- Prefix number for cost center comparison
-------------------------------------------------------------------------------

prefixNumber :: RE' Int
prefixNumber = RE.decimal <* optional (psym (not . isDigit) *> many anySym)

-------------------------------------------------------------------------------
-- Cells
-------------------------------------------------------------------------------

markErrorCell :: MonadState Bool m => m ()
markErrorCell = id .= False

cell_ :: MonadState Bool m => HtmlT (State Bool) () -> HtmlT m ()
cell_ html = case runState (commuteHtmlT html) True of
    (html', s) -> do
        id %= (&& s)
        td_ [ style_ "background: #fcc; font-weight: bold"  | not s ] html'
