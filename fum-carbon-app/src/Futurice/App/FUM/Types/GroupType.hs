{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.FUM.Types.GroupType (
    GroupType (..),
    groupTypeToText,
    groupTypeFromText,
    -- * Prisms
    _GroupType,
    ) where

import Futurice.Generics
import Futurice.Generics.Enum
import Futurice.Prelude
import Futurice.Lucid.Foundation (ToHtml (..))
import Prelude ()

import qualified Data.Csv as Csv

data GroupType
    = GroupTypeAccess
    | GroupTypeProject
    | GroupTypeServer
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Generic)

-- makeLenses ''GroupType -- we don't need prisms atm
deriveGeneric ''GroupType
-- TODO: remove when we drop support for GHC-7.10
deriveLift ''GroupType

ei :: EnumInstances GroupType
ei = sopEnumInstances $
    K "access" :*
    K "project" :*
    K "server" :*
    Nil

-------------------------------------------------------------------------------
-- Boilerplate
-------------------------------------------------------------------------------

groupTypeToText :: GroupType -> Text
groupTypeToText = enumToText ei

groupTypeFromText :: Text -> Maybe GroupType
groupTypeFromText = enumFromText ei

_GroupType :: Prism' Text GroupType
_GroupType = enumPrism ei

instance NFData GroupType

instance Arbitrary GroupType where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance ToHtml GroupType where
    toHtmlRaw = toHtml
    toHtml = toHtml . enumToText ei

instance ToParamSchema GroupType where
    toParamSchema = enumToParamSchema ei

instance ToSchema GroupType where
    declareNamedSchema = enumDeclareNamedSchema ei

instance ToJSON GroupType where
    toJSON = enumToJSON ei

instance FromJSON GroupType where
    parseJSON = enumParseJSON ei

instance FromHttpApiData GroupType where
    parseUrlPiece = enumParseUrlPiece ei

instance ToHttpApiData GroupType where
    toUrlPiece = enumToUrlPiece ei

instance Csv.ToField GroupType where
    toField = enumCsvToField ei

instance Csv.FromField GroupType where
    parseField = enumCsvParseField ei
