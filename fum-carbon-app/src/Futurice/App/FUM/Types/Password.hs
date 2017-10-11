{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.FUM.Types.Password where

import Data.Time                 (addDays)
import Futurice.Generics
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Prelude ()
import System.POSIX.Crypt.SHA512 (cryptSHA512')

import qualified Data.Text as T

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data Password = Password
    { _passwordHash    :: !Text
    , _passwordExpires :: !UTCTime
    }
  deriving (Eq, Ord, Show)

makeLenses ''Password
deriveGeneric ''Password

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance NFData Password where
    rnf (Password h e) = rnf h `seq` rnf e

instance ToJSON Password where
    toJSON = sopToJSON
    toEncoding = sopToEncoding

instance FromJSON Password where
    parseJSON = sopParseJSON

-------------------------------------------------------------------------------
-- Html
-------------------------------------------------------------------------------

passwordToHtml :: Monad m => UTCTime -> Password -> HtmlT m ()
passwordToHtml now (Password h expires) = do
    if now < expires
    then do
        "Expires at "
        toHtml $ formatHumanHelsinkiTime expires
    else span_ [ class_ "alert" ] $ do
        "Expired at "
        toHtml $ formatHumanHelsinkiTime expires
    ": "
    code_ $ toHtml $ T.take 40 h <> "..."

-------------------------------------------------------------------------------
-- Policy
-------------------------------------------------------------------------------

makePassword :: UTCTime -> IO Password
makePassword now = do
    let salt :: ByteString
        salt = "abcdefghijkl"

    let password :: ByteString
        password = "Secret-password"

    let h = decodeUtf8Lenient $ cryptSHA512' (Just 10000) password salt

    pure Password { _passwordHash = h, _passwordExpires = addYear now }
  where
    addYear :: UTCTime -> UTCTime
    addYear (UTCTime d n) = UTCTime (addDays 365 d) n
