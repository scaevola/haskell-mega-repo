{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.FUM.Pages.CreateEmployee (createEmployeePage) where

import Futurice.IdMap   (IdMap)
import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.Command
import Futurice.App.FUM.Markup
import Futurice.App.FUM.Types

import qualified Personio  as P
import qualified Data.Text as T

createEmployeePage
    :: AuthUser
    -> World                    -- ^ the world
    -> IdMap P.Employee  -- ^ employees
    -> P.Employee
    -> HtmlPage "create-employee"
createEmployeePage auth world _es e = fumPage_ "Create employee" auth $ do
    -- Title
    fumHeader_ "Create employee" [] -- TODO: name

    fullRow_ $ table_ $ tbody_ $ do
        vertRow_ "Name" $
            toHtml $ e ^. P.employeeFirst <> " " <> e ^. P.employeeLast
        vertRow_ "Personio id" $
            toHtml $ e ^. P.employeeId
        vertRow_ "Hiring date" $
            maybe "-" (toHtml . show) $ e ^. P.employeeHireDate
        vertRow_ "Contract end date" $
            maybe "-" (toHtml . show) $ e ^. P.employeeEndDate

    -- Form
    commandHtmlSubmit (Proxy :: Proxy CreateEmployee) "Create" "success" $
        vJust (e ^. P.employeeId) :*
        vNothing :*
        maybe vNothing vJust (listToMaybe unused) :*
        vNothing :*
        V (e ^? P.employeeFullname) [] :*
        V (e ^. P.employeeEmail) [] :*
        Nil

    unless (null used) $ do
        hr_ []
        fullRow_ $ do
            em_ "Used logins: "
            forWith_ ";" used $ \ue -> do
                toHtml $ ue ^. employeeName
                " ("
                toHtml $ ue ^. employeeLogin
                ")"
  where
    generateLogins :: Text -> Text -> [Login]
    generateLogins a b
        | T.null a || T.null b = []
        | otherwise = mapMaybe (parseLogin . T.take 4 . (`T.append` b'))
            $ tail $ T.inits a'
      where
        a' = T.toLower a
        b' = T.toLower b

    (used, unused') = spanMaybe (\l -> world ^? worldEmployees . ix l) $
        generateLogins (e ^. P.employeeFirst) (e ^. P.employeeLast)

    unused = nub $ maybeToList (e ^. P.employeeLogin) ++ unused'

spanMaybe :: (a -> Maybe b) -> [a] -> ([b],[a])
spanMaybe _ xs@[] =  ([], xs)
spanMaybe p xs@(x:xs') = case p x of
    Just y  -> let (ys, zs) = spanMaybe p xs' in (y : ys, zs)
    Nothing -> ([], xs)
