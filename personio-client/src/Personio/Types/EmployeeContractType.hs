{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Personio.Types.EmployeeContractType ( 
    ContractType (..), 
    contractTypeToText, 
    contractTypeFromText, 
    _ContractType
    ) where

import Futurice.Generics
import Futurice.Generics.Enum
import Futurice.Prelude

data ContractType
    = PermanentAllIn
    | Permanent
    | FixedTerm
    deriving (Eq, Ord, Show, Read, Typeable, Enum, Bounded, Generic)

makePrisms ''ContractType
deriveGeneric ''ContractType
deriveLift ''ContractType

instance NFData ContractType

instance Arbitrary ContractType where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance ToJSON ContractType where
    toJSON = enumToJSON ei

instance FromJSON ContractType where
    parseJSON = enumParseJSON ei 

ei :: EnumInstances ContractType
ei = sopEnumInstances $ 
    K "permanent all-in" :*
    K "permanent"        :*
    K "fixed term"       :*
    Nil

-------------------------------------------------------------------------------
-- Boilerplate
-------------------------------------------------------------------------------

_ContractType :: Prism' Text ContractType
_ContractType = enumPrism ei

contractTypeFromText :: Text -> Maybe ContractType
contractTypeFromText = enumFromText ei

contractTypeToText :: ContractType -> Text
contractTypeToText = enumToText ei
