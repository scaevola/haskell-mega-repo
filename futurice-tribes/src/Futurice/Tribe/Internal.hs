{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.Tribe.Internal (module Futurice.Tribe.Internal) where

import Data.Aeson.Compat (FromJSON (..), withObject, (.!=), (.:), (.:?))
import Futurice.Office   (Office)
import Futurice.Prelude
import Prelude ()

data TribeInfo = TribeInfo
    { tiName    :: !Text
    , tiOffices :: [Office]
    , tiAliases :: [Text]
    }
  deriving (Eq, Show)

instance FromJSON TribeInfo where
    parseJSON = withObject "TribeInfo" $ \obj -> TribeInfo
        <$> obj .: "name"
        <*> obj .:? "offices" .!= []
        <*> obj .:? "aliases" .!= []

-- TODO: remove when we drop support for GHC-7.10
deriveLift ''TribeInfo
