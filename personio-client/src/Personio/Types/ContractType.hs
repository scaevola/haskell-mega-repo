{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Personio.Types.ContractType ( 
    ContractType (..), 
    contractTypeToText, 
    contractTypeFromText, 
    _ContractType
    ) where

import Futurice.Generics
import Futurice.Generics.Enum
import Futurice.Prelude
import Lucid                  (ToHtml (..))

data ContractType
    = PermanentAllIn
    | Permanent
    | FixedTerm
    deriving (Eq, Ord, Show, Read, Typeable, Enum, Bounded, Generic)

makePrisms ''ContractType
deriveGeneric ''ContractType
deriveLift ''ContractType

ei :: EnumInstances ContractType
ei = sopEnumInstances $ 
    K "permanent all-in" :*
    K "permanent"        :*
    K "fixed term"       :*
    Nil

-------------------------------------------------------------------------------
-- Boilerplate
-------------------------------------------------------------------------------

contractTypeToText :: ContractType -> Text
contractTypeToText = enumToText ei

contractTypeFromText :: Text -> Maybe ContractType
contractTypeFromText = enumFromText ei

_ContractType :: Prism' Text ContractType
_ContractType = enumPrism ei

instance NFData ContractType

instance Arbitrary ContractType where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance ToHtml ContractType where
    toHtmlRaw = toHtml
    toHtml = toHtml . enumToText ei

instance ToParamSchema ContractType where
    toParamSchema = enumToParamSchema ei

instance ToSchema ContractType where
    declareNamedSchema = enumDeclareNamedSchema ei

instance ToJSON ContractType where
    toJSON = enumToJSON ei

instance FromJSON ContractType where
    parseJSON = enumParseJSON ei

instance FromHttpApiData ContractType where
    parseUrlPiece = enumParseUrlPiece ei

instance ToHttpApiData ContractType where
    toUrlPiece = enumToUrlPiece ei
