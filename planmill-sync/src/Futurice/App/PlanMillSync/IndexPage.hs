{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Futurice.App.PlanMillSync.IndexPage (indexPage) where

import Control.Lens                (IndexedGetting, ifoldMapOf)
import Control.Monad.Writer.CPS    (Writer, runWriter)
import Data.Char                   (isDigit)
import Data.Map.Lens               (toMapOf)
import Data.Maybe                  (isNothing)
import Data.Monoid                 (Any (..))
import Data.Ord                    (Down (..))
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
            td_ "Status"
            td_ "Ext"
            td_ "Contract type"
            td_ "Contract span"

            td_ "PM Superior"
            td_ "Cost center = PM Team"
            td_ "PM Account"
            td_ "PM email"

        tbody_ $ do
            let elements0 = itoListWithOf (ifolded . _These) processBoth employees
            let elements1 = map (runWriter . commuteHtmlT) elements0
            let elements2 = map fst $ sortOn (Down . snd) elements1
            traverse_ id elements2
  where
    processBoth :: MonadWriter Any m => FUM.Login -> (PMUser, P.Employee) -> HtmlT m ()
    processBoth login (pm, p) = tr_ $ do
        let pmu = pmUser pm
        let pmt = pmTeam pm

        td_ $ toHtml login
        td_ $ toHtml $ p ^. P.employeeId
        td_ $ toHtml $ pmu ^. PM.identifier
        td_ $ toHtml $ p ^. P.employeeFullname
        -- td_ $ toHtml $ p ^. P.employeeTribe
        -- td_ $ toHtml $ p ^. P.employeeOffice

        cell_ $ do
            let pActive = P.employeeIsActive now p

            if pActive then "Active" else "Inactive"

            unless (pActive == (p ^. P.employeeStatus `elem` [P.Active, P.Leave])) $
                markPersonioCell $ mconcat
                    [ "Personio status and contract dates disagree"
                    , "Status: "
                    , P.statusToText (p ^. P.employeeStatus)
                    ]

            unless (pActive == (pmPassive pm == "Active")) $ do
                markFixableCell $ mconcat
                    [ "PlanMill active status doesn't agree with Personio status: Personio "
                    , P.statusToText (p ^. P.employeeStatus)
                    , " ≇ PM "
                    , pmPassive pm
                    ]
                " ≠ "
                toHtml $ pmPassive pm

        cell_ $ case p ^. P.employeeEmploymentType of
            Nothing -> markPersonioCell "Personio employee should have employment type set"
            Just P.Internal -> pure ()
            Just P.External -> "Ext"

        -- Contract type
        cell_ $ case p ^. P.employeeContractType of
            Nothing -> markPersonioCell "Personio employee should have contract type set"
            Just pContract -> do
                let pEmploymentType = p ^. P.employeeEmploymentType
                toHtml (show pContract)

                when (pEmploymentType == Just P.External && pContract /= P.FixedTerm) $
                    markPersonioCell "Externals should have contract type FixedTerm"

                unless (contractTypeOk pEmploymentType pContract (pmContract pm)) $ do
                    markErrorCell "Contract types don't agree"
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
            when (isNothing pStart) $ markErrorCell "Employee should have hire date in Personio"
            when (isNothing pmStart) $ markErrorCell "Employee should have hire date in PlanMill"

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
                markErrorCell "Contract dates differ"
                " ≠ "
                toHtml $ formatDateSpan pmStart pmEnd

        -- superior
        cell_ $ for_ (PM.uSuperior pmu) $ \sv -> do
            markFixableCell "PlanMill employee shouldn't have supervisor set"
            toHtml (show sv)

        -- Cost centers
        cell_ $ do
            let planmillCC = match prefixNumber =<< (PM.tName <$> pmt)
            let personioCC = match prefixNumber =<< (p ^. P.employeeCostCenter)
            let ccEqual = planmillCC == personioCC
            traverse_ toHtml $  p ^. P.employeeCostCenter
            unless ccEqual $ do
                markErrorCell "Cost centers should be equal"
                " ≠ "
                traverse_ (toHtml . PM.tName) pmt

        -- PM Account
        cell_ $ case pmAccount pm of
            Nothing -> markErrorCell "PlanMill employee doesn't have account set"
            Just a  -> do
                let name = PM.saName a
                when (name /= officeToAccount (p ^. P.employeeOffice)) $ do
                    markErrorCell "PM Account doesn't agree with Personio Office value"
                    toHtml (p ^. P.employeeOffice)
                    " ≠ "
                toHtml name

        -- PM email
        cell_ $ case PM.uEmail pmu of
            Nothing -> markFixableCell "PlanMill employee doesn't have email set"
            Just e  ->
                if (e == FUM.loginToText login <> "@futurice.com")
                then "OK"
                else do
                    markFixableCell "Email should be `login`@futurice.com"
                    toHtml e

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

contractTypeOk :: Maybe P.EmploymentType -> P.ContractType -> Text -> Bool
contractTypeOk (Just P.External) ct t =
    ct == P.FixedTerm && t == "Subcontractor"
contractTypeOk _ P.Permanent      t = "Permanent" `T.isInfixOf` t
contractTypeOk _ P.PermanentAllIn t = "no working time" `T.isInfixOf` t
contractTypeOk _ P.FixedTerm      t = "Permanent" `T.isInfixOf` t

-------------------------------------------------------------------------------
-- Prefix number for cost center comparison
-------------------------------------------------------------------------------

prefixNumber :: RE' Int
prefixNumber = RE.decimal <* optional (psym (not . isDigit) *> many anySym)

-------------------------------------------------------------------------------
-- Cells
-------------------------------------------------------------------------------

-- | Smaller state: better.
data CellState
    = CellOk
    | CellFixable (NonEmpty Text)     -- ^ Error which is mechanically fixable
    | CellPersonio (NonEmpty Text)    -- ^ personio data is inconsistent
    | CellInconsistent (NonEmpty Text) -- ^ Planmill and personio disagree
  deriving (Eq, Ord, Show)

cellStateErrors :: CellState -> [Text]
cellStateErrors CellOk = []
cellStateErrors (CellFixable xs) = toList xs
cellStateErrors (CellPersonio xs) = toList xs
cellStateErrors (CellInconsistent xs) = toList xs

append :: NonEmpty a -> [a] -> NonEmpty a
append (x :| xs) ys = x :| (xs ++ ys)

instance Semigroup CellState where
    CellInconsistent xs <> s = CellInconsistent (append xs $ cellStateErrors s)
    s <> CellInconsistent xs = CellInconsistent (append xs $ cellStateErrors s)
    CellPersonio xs <> s     = CellPersonio (append xs $ cellStateErrors s)
    s <> CellPersonio xs     = CellPersonio (append xs $ cellStateErrors s)
    CellFixable xs <> s      = CellFixable (append xs $ cellStateErrors s)
    s <> CellFixable xs      = CellFixable (append xs $ cellStateErrors s)
    CellOk <> CellOk         = CellOk

instance Monoid CellState where
    mempty = CellOk
    mappend = (<>)

markFixableCell :: MonadWriter CellState m => Text -> m ()
markFixableCell err = tell (CellFixable (err :| []))

markPersonioCell :: MonadWriter CellState m => Text -> m ()
markPersonioCell err = tell (CellPersonio (err :| []))

markErrorCell :: MonadWriter CellState m => Text -> m ()
markErrorCell err = tell (CellInconsistent (err :| []))

cell_ :: MonadWriter Any m => HtmlT (Writer CellState) () -> HtmlT m ()
cell_ html = case runWriter (commuteHtmlT html) of
    (html', CellOk)              -> td_ html'
    (html', CellFixable xs)      -> do
        tell (Any True)
        td_ [ style_ "background: #ccf; font-weight: bold", errorsTitle_ xs ] html'
    (html', CellPersonio xs)     -> do
        tell (Any True)
        td_ [ style_ "background: #ffc; font-weight: bold", errorsTitle_ xs ] html'
    (html', CellInconsistent xs) -> do
        tell (Any True)
        td_ [ style_ "background: #fcc; font-weight: bold", errorsTitle_ xs ] html'

errorsTitle_ :: NonEmpty Text -> Attribute
errorsTitle_ xs = title_ $ T.intercalate "; " $ toList xs

instance MonadWriter w m => MonadWriter w (HtmlT m) where
    tell = lift . tell
    listen = undefined
    pass = undefined


