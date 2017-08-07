{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Personio.Types.EmployeeStatus (
    Status(..),
    statusToText,
    statusFromText,
    _Status,
    ) where

import Futurice.Generics
import Futurice.Generics.Enum
import Futurice.Prelude
import Lucid                  (ToHtml (..))
import Prelude ()

-- | Employee contractual status with initial state being Onboarding
-- Personio changes status to Inactive on reaching Employee.endDate
data Status
    = Active
    | Inactive
    | Onboarding
    | Leave
    deriving (Eq, Ord, Show, Read, Typeable, Enum, Bounded, Generic)

makePrisms ''Status
deriveGeneric ''Status
deriveLift ''Status

ei :: EnumInstances Status
ei = sopEnumInstances $
    K "Active" :*
    K "Inactive" :*
    K "Onboarding" :*
    K "Leave" :*
    Nil

-------------------------------------------------------------------------------
-- Boilerplate
-------------------------------------------------------------------------------

statusToText :: Status -> Text
statusToText = enumToText ei

statusFromText :: Text -> Maybe Status
statusFromText = enumFromText ei

_Status :: Prism' Text Status
_Status = enumPrism ei

instance NFData Status

instance Arbitrary Status where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance ToHtml Status where
    toHtmlRaw = toHtml
    toHtml = toHtml . enumToText ei

instance ToParamSchema Status where
    toParamSchema = enumToParamSchema ei

instance ToSchema Status where
    declareNamedSchema = enumDeclareNamedSchema ei

instance ToJSON Status where
    toJSON = enumToJSON ei

instance FromJSON Status where
    parseJSON = enumParseJSON ei

instance FromHttpApiData Status where
    parseUrlPiece = enumParseUrlPiece ei

instance ToHttpApiData Status where
    toUrlPiece = enumToUrlPiece ei

