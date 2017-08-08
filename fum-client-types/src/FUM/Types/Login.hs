{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module FUM.Types.Login (
    Login,
    mkLogin,
    parseLogin,
    parseLogin',
    InvalidLoginFormat (..),
    ) where

import Prelude
import Futurice.Prelude
import Futurice.Generics
import Language.Haskell.TH     (ExpQ)
import Lucid                   (ToHtml (..))

import qualified Data.Aeson.Compat as Aeson
import qualified Data.Swagger      as Swagger
import qualified Data.Text         as T
import qualified Test.QuickCheck   as QC

newtype Login = Login Text
  deriving (Eq, Ord)

deriveLift ''Login

instance Show Login where
    showsPrec _ (Login l)
        = showString "$(mkLogin "
        . showsPrec 11 l
        . showString ")"

-------------------------------------------------------------------------------
-- Magic
-------------------------------------------------------------------------------

mkLogin :: Text -> ExpQ
mkLogin t = do
    l <- parseLogin t
    [| l |]

parseLogin :: MonadThrow m => Text -> m Login
parseLogin = either (throwM . InvalidLoginFormat) pure . parseLogin'

newtype InvalidLoginFormat = InvalidLoginFormat String
  deriving (Show, Typeable)

instance Exception InvalidLoginFormat

-------------------------------------------------------------------------------
-- Parse
-------------------------------------------------------------------------------

loginToText :: Login -> Text
loginToText (Login l) = l

parseLogin' :: Text -> Either String Login
parseLogin' t
    | not (4 <= len && len <= 5) = Left $ "login of invalid length: " ++ show len
    | T.any isInvalidChar t      = Left $ "login with invalid characters: " ++ show (T.take 3 (T.filter isInvalidChar t))
    | otherwise                  = Right (Login t)
  where
    len = T.length t
    isInvalidChar = (`notElem` ['a'..'z'])

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance Hashable Login where
    hashWithSalt salt (Login l) = hashWithSalt salt l

instance NFData Login where
    rnf (Login i) = rnf i

instance Arbitrary Login where
    arbitrary = QC.elements $ map Login [ "fooo", "booo", "hooo" ]

instance ToHtml Login where
    toHtmlRaw = toHtml
    toHtml = toHtml . loginToText

instance ToParamSchema Login where
    toParamSchema _ = mempty
        & Swagger.type_ .~ Swagger.SwaggerString
        -- & Swagger.enum_ ?~ map enumToJSON_ enumUniverse_

instance ToSchema Login where
    declareNamedSchema p = pure $ Swagger.NamedSchema (Just "Login") $ mempty
        & Swagger.paramSchema .~ toParamSchema p

instance ToJSON Login where
    toJSON = Aeson.String . loginToText

instance FromJSON Login where
    parseJSON = Aeson.withText "Login" $
        either (fail . view unpacked) pure . parseLogin'

instance FromHttpApiData Login where
    parseUrlPiece = first (view packed) . parseLogin'

instance ToHttpApiData Login where
    toUrlPiece = loginToText
