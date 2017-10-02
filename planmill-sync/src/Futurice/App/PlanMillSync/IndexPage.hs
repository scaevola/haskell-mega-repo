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
import Futurice.Office             (Office (..))
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
            -- td_ "Tribe"
            -- td_ "Office"
            td_ "Ext"
            td_ "Contract type"
            td_ "Contract span"

            td_ "PM Superior"
            td_ "Cost center = PM Team"
            td_ "PM Account"
            td_ "PM email"

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
        -- td_ $ toHtml $ p ^. P.employeeTribe
        -- td_ $ toHtml $ p ^. P.employeeOffice

        cell_ $ case p ^. P.employeeEmploymentType of
            Nothing -> markErrorCell
            Just P.Internal -> pure ()
            Just P.External -> "Ext"

        -- Contract type
        cell_ $ case p ^. P.employeeContractType of
            Nothing -> markErrorCell
            Just pContract -> do
                toHtml (show pContract)

                when (p ^. P.employeeEmploymentType == Just P.External && pContract /= P.FixedTerm) $
                    markErrorCell

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

        -- PM Account
        cell_ $ case pmAccount pm of
            Nothing -> markErrorCell
            Just a  -> do
                let name = PM.saName a
                when (name /= officeToAccount (p ^. P.employeeOffice)) $ do
                    markErrorCell
                    toHtml (p ^. P.employeeOffice)
                    " ≠ "
                toHtml name

        -- PM email
        cell_ $ case PM.uEmail pmu of
            Nothing -> markErrorCell
            Just e  ->
                if (e == FUM.loginToText login <> "@futurice.com")
                then "OK"
                else markErrorCell >> toHtml e

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
-------------------------------------------------------------------------------
-- Account
-------------------------------------------------------------------------------

officeToAccount :: Office -> Text
officeToAccount OffHelsinki  = "Futurice Oy"
officeToAccount OffTampere   = "Futurice Oy"
officeToAccount OffBerlin    = "Futurice GmbH"
officeToAccount OffMunich    = "Futurice GmbH"
officeToAccount OffLondon    = "Futurice Ltd"
officeToAccount OffStockholm = "Futu Sweden AB"
officeToAccount OffOther     = "???"

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
