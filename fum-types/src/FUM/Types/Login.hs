{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module FUM.Types.Login (
    Login,
    loginToText,
    mkLogin,
    parseLogin,
    parseLogin',
    loginRegexp,
    InvalidLoginFormat (..),
    ) where

import Data.Aeson.Types
       (FromJSONKey (..), FromJSONKeyFunction (..), ToJSONKey (..),
       toJSONKeyText)
import Futurice.Generics
import Futurice.Prelude
import Language.Haskell.TH         (ExpQ)
import Lucid                       (ToHtml (..))
import Prelude ()
import Text.Regex.Applicative.Text (RE', psym)

import qualified Data.Aeson.Compat                    as Aeson
import qualified Data.Csv                             as Csv
import qualified Data.Swagger                         as Swagger
import qualified Data.Text                            as T
import qualified Database.PostgreSQL.Simple.FromField as Postgres
import qualified Database.PostgreSQL.Simple.ToField   as Postgres
import qualified Test.QuickCheck                      as QC

-- | Login name. @[a-z]{4,5}@.
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

-- | Parse login identifier
parseLogin' :: Text -> Either String Login
parseLogin' t
    | not (4 <= len && len <= 5) = Left $ "login of invalid length: " ++ show len
    | T.any isInvalidChar t      = Left $ "login with invalid characters: " ++ show (T.take 3 (T.filter isInvalidChar t))
    | otherwise                  = Right (Login t)
  where
    len = T.length t
    isInvalidChar = (`notElem` ['a'..'z'])

-- | Regexp for login identifier
--
-- /Note:/ use `parseLogin` if possible, as it provides better errors.
loginRegexp :: RE' Login
loginRegexp = Login . T.pack <$> range 4 5 (psym (`elem` ['a'..'z']))
  where
    range
        :: Alternative f
        => Int  -- ^ min
        -> Int  -- ^ max
        -> f a
        -> f [a]
    range mi ma f = go mi ma
      where
        go start end
            | start > end || end <= 0 = pure []
            | start > 0 = (:) <$> f <*> go (start - 1) (end - 1)
            | otherwise = inRange <$> optional f <*> go 0 (end - 1)

        inRange current next = maybe [] (:next) current

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
        either fail pure . parseLogin'

instance FromHttpApiData Login where
    parseUrlPiece = first (view packed) . parseLogin'

instance ToHttpApiData Login where
    toUrlPiece = loginToText

instance ToJSONKey Login where
    toJSONKey = toJSONKeyText loginToText

instance FromJSONKey Login where
    fromJSONKey = FromJSONKeyTextParser $
        either fail pure . parseLogin'

instance Csv.ToField Login where
    toField = Csv.toField . loginToText

instance Postgres.ToField Login where
    toField = Postgres.toField . loginToText

instance Postgres.FromField Login where
    fromField f mdata = do
        t <- Postgres.fromField f mdata
        either fail pure (parseLogin' t)
