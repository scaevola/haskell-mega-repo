{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist (defaultMain) where

import Futurice.Prelude
import Prelude ()

import Control.Lens              (to)
import Futurice.Servant
import Lucid                     hiding (for_)
import Lucid.Foundation.Futurice
import Servant
import Test.QuickCheck           (arbitrary, generate, resize)

import Futurice.App.Checklist.API
import Futurice.App.Checklist.Clay
import Futurice.App.Checklist.Types

-- TODO: make to .Types.Ctx
type Ctx = ()

server :: Ctx -> Server ChecklistAPI
server _ = liftIO indexPage

currentDay :: IO Day
currentDay = utctDay <$> currentTime

locHtml :: Monad m => Location -> HtmlT m ()
locHtml l = a_ [ href_ "#" ] $ case l of
    LocHelsinki  -> "Hel"
    LocTampere   -> "Tre"
    LocBerlin    -> "Ber"
    LocLondon    -> "Lon"
    LocStockholm -> "Sto"
    LocMunich    -> "Mun"
    LocOther     -> "Oth"

indexPage :: IO (Page "indexpage")
indexPage = do
    today <- currentDay
    world <- generate (resize 200 arbitrary)
    let users = world ^. worldUsers
    pure $ Page $ page_ "Checklist" pageParams $ table_ $ do
        thead_ $ tr_ $ do
            th_ "Sts"
            th_ "Loc"
            th_ "Name"
            th_ "List"
            th_ "Starts"
            th_ "Confirmed"
            th_ "ETA"
            th_ "Group items?"
            th_ "Items"
        tbody_ $ for_ users $ \user -> do
            let eta = toModifiedJulianDay today - toModifiedJulianDay (user ^. userStartingDay)
            tr_ [class_ $ etaClass eta] $ do
                td_ "X"
                td_ $ locHtml $ user ^. userLocation
                -- TODO: use safeLink
                td_ $ a_ [href_ $ "/user/" <> user ^. identifier ^. to identifierToText ] $ toHtml $
                    user ^. userFirstName <> " " <> user ^. userLastName
                td_ "TODO"
                td_ $ toHtml $ show $ user ^. userStartingDay
                td_ $ toHtml $ show $ user ^. userConfirmed
                td_ $ toHtml $ show eta
                td_ "TODO"
                td_ "TODO"
  where
    etaClass eta = case compare 0 eta of
        EQ -> "eta-today"
        LT -> "eta-past"
        GT -> "eta-future"

defaultMain :: IO ()
defaultMain = futuriceServerMain
    "Checklist API"
    "Super TODO"
    (Proxy :: Proxy ('FutuAccent 'AF4 'AC3))
    (pure ()) (const 8000) -- getConfig cfgPort
    checklistApi server futuriceNoMiddleware
    $ \_ _cache -> -- do
        pure ()
