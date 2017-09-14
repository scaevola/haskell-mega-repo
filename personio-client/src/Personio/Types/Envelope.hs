{-# LANGUAGE OverloadedStrings #-}
module Personio.Types.Envelope where

import Data.Aeson.Compat
import Data.Aeson.Types    (FromJSON1 (..), explicitParseField, parseJSON1)
import Futurice.Aeson
import Futurice.Prelude
import Prelude ()

-- | API returns data in the envelope
newtype Envelope a = Envelope { getEnvelope :: a }

instance FromJSON a => FromJSON (Envelope a) where
    parseJSON = parseJSON1

instance FromJSON1 Envelope where
    liftParseJSON p _ = withObjectDump "Envelope" $ \obj -> do
        b <- obj .: "success"
        case b of
            False -> do
                err <- obj .: "error"
                fail (errMessage err ^. unpacked)
            True -> Envelope <$> explicitParseField p obj "data"

-- | API error.
data Err = Err
    { errCode    :: !Int
    , errMessage :: !Text
    }

instance FromJSON Err where
    parseJSON = withObjectDump "Error" $ \obj -> Err
        <$> obj .: "code"
        <*> obj .: "message"
