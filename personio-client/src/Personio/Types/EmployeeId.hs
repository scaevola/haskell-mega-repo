{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Personio.Types.EmployeeId where

import Data.Aeson.Compat
import Futurice.Constants (personioPublicUrl)
import Futurice.Generics
import Futurice.Prelude
import Lucid              (ToHtml (..), a_, class_, href_)
import Prelude ()

import qualified Data.Csv as Csv

-- | Personio employee id.
newtype EmployeeId = EmployeeId Word
  deriving (Eq, Ord, Show)

deriveGeneric ''EmployeeId

instance Arbitrary EmployeeId where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance Hashable EmployeeId where
    hashWithSalt salt (EmployeeId i) = hashWithSalt salt i

instance FromJSON EmployeeId where
    parseJSON = fmap EmployeeId . parseJSON

instance ToJSON EmployeeId where
    toJSON (EmployeeId i) = toJSON i
    toEncoding (EmployeeId i) = toEncoding i

instance NFData EmployeeId where
    rnf (EmployeeId i) = rnf i

instance ToHtml EmployeeId where
    toHtmlRaw = toHtml
    toHtml (EmployeeId i) = do
        let t = textShow i
        a_ [ class_ "personio", href_ $ personioPublicUrl <> "/staff/details/" <> t ] $
            toHtml t

-- | We could use 'GeneralizedNewtypeDeriving', but we don't (yet?).
instance ToParamSchema EmployeeId where
    toParamSchema = newtypeToParamSchema

instance ToSchema EmployeeId where
    declareNamedSchema = newtypeDeclareNamedSchema

instance FromHttpApiData EmployeeId where
    parseUrlPiece = newtypeParseUrlPiece

instance ToHttpApiData EmployeeId where
    toUrlPiece = newtypeToUrlPiece

instance Csv.ToField EmployeeId where
    toField (EmployeeId i) = Csv.toField i

_EmployeeId :: Prism' Text EmployeeId
_EmployeeId = prism' toUrlPiece (either (const Nothing) Just . parseUrlPiece)
