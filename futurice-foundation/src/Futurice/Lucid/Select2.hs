{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.Lucid.Select2 (
    Select2Data (..),
    Select2Result (..),
    ) where

import Data.Aeson        (object, pairs, (.=))
import Data.Swagger      (NamedSchema (..))
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

-- | See <https://select2.org/data-sources/formats>.
data Select2Data = Select2Data
    { s2dId   :: !Text
    , s2dText :: !Text
    }
  deriving (Eq, Ord, Show, Typeable, Generic)

deriveGeneric ''Select2Data

instance ToJSON Select2Data where
    toJSON = sopToJSON
    toEncoding = sopToEncoding
instance FromJSON Select2Data where parseJSON          = sopParseJSON
instance ToSchema Select2Data where declareNamedSchema = sopDeclareNamedSchema

-- | See <https://select2.org/data-sources/formats>.
newtype Select2Result = Select2Result [Select2Data]
  deriving (Eq, Ord, Show, Typeable, Generic)

deriveGeneric ''Select2Result

instance ToJSON Select2Result where
    toJSON (Select2Result xs) = object [ "results" .= xs ]
    toEncoding (Select2Result xs) = pairs $ "results" .= xs

-- | TODO: incorrect
instance ToSchema Select2Result where
    declareNamedSchema _ = return $ NamedSchema (Just "Select2 Result") mempty
