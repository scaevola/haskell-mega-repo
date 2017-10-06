{-# LANGUAGE OverloadedStrings #-}
module Futurice.Email (
    Email,
    emailKleene,
    emailRe,
    ) where

import Data.Aeson
       (FromJSON (..), FromJSONKey (..), FromJSONKeyFunction (..), ToJSON (..),
       ToJSONKey (..), withText)
import Futurice.Prelude
import Kleene                      (Kleene, kleeneEverything, kleeneToRe)
import Lucid                       (ToHtml (..), a_, href_)
import Prelude ()
import Text.Regex.Applicative.Text (RE', match)
import Web.HttpApiData             (FromHttpApiData (..), ToHttpApiData (..))

import qualified Data.Csv     as Csv
import qualified Data.Swagger as S

-- | Futurice email. i.e. @someone@@futurice.com@.
newtype Email = Email Text
  deriving (Eq, Ord, Show)

emailToText :: Email -> Text
emailToText (Email x) = x <> suffix

emailFromText :: Text -> Maybe Email
emailFromText = match emailRe

parseEmail :: Monad m => Text -> m Email
parseEmail t = maybe
    (fail $ "Invalid Futurice email: " <> show t)
    pure
    (emailFromText t)

-------------------------------------------------------------------------------
-- instances
-------------------------------------------------------------------------------

instance NFData Email where
    rnf (Email x) = rnf x

instance Hashable Email where
    hashWithSalt salt (Email x) = hashWithSalt salt x

instance ToHtml Email where
    toHtmlRaw = toHtml
    toHtml (Email x) = a_ [ href_ $ "mailto:" <> x' ] $ toHtml x'
      where
        x' = x <> suffix

instance S.ToParamSchema Email where
    toParamSchema _ = mempty
        & S.type_  .~ S.SwaggerString
        & S.format ?~ "futurice-email"

instance S.ToSchema Email where
    declareNamedSchema p = pure $ S.NamedSchema (Just "Futurice email") $
        S.paramSchemaToSchema p

instance ToJSON Email where
    toJSON = toJSON . emailToText

instance FromJSON Email where
    parseJSON = withText "Email" parseEmail

instance ToJSONKey Email where
    toJSONKey = emailToText >$< toJSONKey

instance FromJSONKey Email where
    fromJSONKey = FromJSONKeyTextParser parseEmail

instance ToHttpApiData Email where
    toUrlPiece = emailToText

instance FromHttpApiData Email where
    parseUrlPiece = parseEmail

instance Csv.ToField Email where
    toField = Csv.toField . emailToText

instance Csv.FromField Email where
    parseField x = Csv.parseField x >>= parseEmail

-------------------------------------------------------------------------------
-- helpers
-------------------------------------------------------------------------------

emailKleene :: Kleene Char Email
emailKleene = Email . view packed
    <$> kleeneEverything
    <* (suffix :: Kleene Char String)

emailRe :: RE' Email
emailRe = kleeneToRe emailKleene

suffix :: IsString a => a
suffix = "@futurice.com"
