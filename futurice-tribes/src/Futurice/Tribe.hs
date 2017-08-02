{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.Tribe (
    Tribe,
    tribeToText,
    tribeFromText,
    _Tribe,
    tribeOffices,
    ) where

import Futurice.Generics
import Lucid (ToHtml (..))
import Futurice.Office
import Futurice.Prelude
import Futurice.Tribe.Internal
import Prelude ()

import qualified Data.Aeson.Compat    as Aeson
import qualified Data.Map             as Map
import qualified Data.Swagger         as Swagger
import qualified Data.Text            as T
import qualified Data.Vector          as V

-- | Tribe.
newtype Tribe = Tribe Int

-------------------------------------------------------------------------------
-- Magic
-------------------------------------------------------------------------------

-- | Vector to have constant time lookups.
tribeInfos :: Vector TribeInfo
tribeInfos
    = V.fromList
    $ sortOn tiName
    $ $(makeRelativeToProject "tribes.json" >>= embedFromJSON (Proxy :: Proxy [TribeInfo]))

-- | Map from lowercased tribe names and aliases to index in tribeInfos and TribeInfo itself
tribeLookup :: Map Text (Int, TribeInfo)
tribeLookup
    = Map.fromList
    $ concatMap f
    $ zip [0..]
    $ toList tribeInfos
  where
    f (i, ti) = [ (T.toLower k, (i, ti)) | k <- tiName ti : tiAliases ti ]

-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

tribeInfo :: Tribe -> TribeInfo
tribeInfo (Tribe i)
    = fromMaybe (error "tribeInfo: invalid Tribe")
    $ tribeInfos ^? ix i

tribeName :: Tribe -> Text
tribeName = tiName . tribeInfo

tribeOffices :: Tribe -> [Office]
tribeOffices = tiOffices . tribeInfo

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

tribeToText :: Tribe -> Text
tribeToText = tribeName

tribeFromText :: Text -> Maybe Tribe
tribeFromText k = Tribe <$> tribeLookup ^? ix (T.toLower k) . _1

tribeFromTextE :: Text -> Either String Tribe
tribeFromTextE k =
    maybe (Left $ "Invalid tribe " ++ show k) Right (tribeFromText k)

_Tribe :: Prism' Text Tribe
_Tribe = prism' tribeToText tribeFromText

instance NFData Tribe where
    rnf (Tribe i) = rnf i

instance ToHtml Tribe where
    toHtmlRaw = toHtml
    toHtml = toHtml . tribeToText

instance ToParamSchema Tribe where
    toParamSchema _ = mempty
        & Swagger.type_ .~ Swagger.SwaggerString
        -- & Swagger.enum_ ?~ map enumToJSON_ enumUniverse_

instance ToSchema Tribe where
    declareNamedSchema p = pure $ Swagger.NamedSchema (Just "Tribe") $ mempty
        & Swagger.paramSchema .~ toParamSchema p

instance ToJSON Tribe where
    toJSON = Aeson.String . tribeToText

instance FromJSON Tribe where
    parseJSON = Aeson.withText "Tribe" $
        either (fail . view unpacked) pure . tribeFromTextE

instance FromHttpApiData Tribe where
    parseUrlPiece = first (view packed) . tribeFromTextE

instance ToHttpApiData Tribe where
    toUrlPiece = tribeToText
