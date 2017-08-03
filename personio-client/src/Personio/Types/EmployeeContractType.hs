{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Personio.Types.EmployeeContractType
    (ContractType (..), contractTypeFromText
    ) where

import Data.Aeson.Compat (Value (String), withText)
import Futurice.Generics
import Futurice.Prelude

import qualified Data.Map as Map
import qualified Data.Text as T

data ContractType
    = PermanentAllIn
    | Permanent
    | FixedTerm
    deriving (Eq, Ord, Show, Read, Typeable, Enum, Bounded, Generic)

makePrisms ''ContractType
deriveGeneric ''ContractType

instance NFData ContractType

instance Arbitrary ContractType where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance ToJSON ContractType where
    toJSON = String . contractTypeToText

instance FromJSON ContractType where
    parseJSON = withText "Contract type" $ \t ->
        maybe (fail $ "invalid Contract type" <> t ^. unpacked) pure
        $ t ^? _ContractType

contractTypeFromText :: Text -> Maybe ContractType
contractTypeFromText t = Map.lookup (T.toLower t) m
  where
    m = Map.fromList $
        map (\x -> (T.toLower $ contractTypeToText x, x))
        [minBound .. maxBound]

contractTypeToText :: ContractType -> Text
contractTypeToText PermanentAllIn = "Permanent all-in"
contractTypeToText Permanent      = "Permanent"
contractTypeToText FixedTerm      = "Fixed term"
