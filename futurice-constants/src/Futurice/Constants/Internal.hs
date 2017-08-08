{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.Constants.Internal (module Futurice.Constants.Internal) where

import Data.Aeson.Compat (FromJSON (..), withObject, (.:))
import Futurice.Prelude
import Prelude ()

data Constants = Constants
    { fumPublicUrl      :: Text
    , personioPublicUrl :: Text
    }
  deriving (Eq, Show)

instance FromJSON Constants where
    parseJSON = withObject "Constants" $ \obj -> Constants
        <$> obj .: "fumPublicUrl"
        <*> obj .: "personioPublicUrl"

-- TODO: remove when we drop support for GHC-7.10
deriveLift ''Constants
