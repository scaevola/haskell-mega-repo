{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Personio.Types.EmployeeEmploymentType (
    EmploymentType (..),
    employmentTypeToText,
    employmentTypeFromText,
    _EmploymentType,
    ) where

import Futurice.Generics
import Futurice.Generics.Enum
import Futurice.Prelude
import Lucid                  (ToHtml (..))

data EmploymentType
    = Internal
    | External
    deriving (Eq, Ord, Show, Read, Typeable, Enum, Bounded, Generic)

makePrisms ''EmploymentType
deriveGeneric ''EmploymentType
deriveLift ''EmploymentType

ei :: EnumInstances EmploymentType
ei = sopEnumInstances $
    K "internal" :*
    K "external" :*
    Nil

-------------------------------------------------------------------------------
-- Boilerplate
-------------------------------------------------------------------------------

employmentTypeToText :: EmploymentType -> Text
employmentTypeToText = enumToText ei

employmentTypeFromText :: Text -> Maybe EmploymentType
employmentTypeFromText = enumFromText ei

_EmploymentType :: Prism' Text EmploymentType
_EmploymentType = enumPrism ei

instance NFData EmploymentType

instance Arbitrary EmploymentType where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance ToHtml EmploymentType where
    toHtmlRaw = toHtml
    toHtml = toHtml . enumToText ei

instance ToParamSchema EmploymentType where
    toParamSchema = enumToParamSchema ei

instance ToSchema EmploymentType where
    declareNamedSchema = enumDeclareNamedSchema ei

instance ToJSON EmploymentType where
    toJSON = enumToJSON ei

instance FromJSON EmploymentType where
    parseJSON = enumParseJSON ei

instance FromHttpApiData EmploymentType where
    parseUrlPiece = enumParseUrlPiece ei

instance ToHttpApiData EmploymentType where
    toUrlPiece = enumToUrlPiece ei

