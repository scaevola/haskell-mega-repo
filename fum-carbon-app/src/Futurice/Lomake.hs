{-# LANGUAGE CPP                     #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE UndecidableSuperClasses #-}
#endif

-- | Simple but awesome form library.
--
-- /TODO:/ mode to @futurice-prelude@ after stabilised.
module Futurice.Lomake (
    module Futurice.Lomake,
    -- * Re-exports
    SOP.IsProductType,
    ) where

import Control.Lens               (use)
import Control.Monad.Fail         (MonadFail)
import Control.Monad.State.Strict
import Data.Char                  (isAlphaNum)
import Data.Maybe                 (isNothing)
import Data.Swagger               (NamedSchema (..))
import Futurice.Generics
import Futurice.List              (UnSingleton)
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Prelude ()
import Servant.API                (Link)

-- import Generics.SOP              hiding (FieldName)

import qualified Data.Aeson.Compat   as Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T
import qualified Generics.SOP        as SOP

-------------------------------------------------------------------------------
-- FieldName
-------------------------------------------------------------------------------

type FieldName = Text

class HasFieldName a where
    fieldName :: Lens' a FieldName

-------------------------------------------------------------------------------
-- Field
-------------------------------------------------------------------------------

data Field a where
    TextField   :: TextFieldOptions a -> Field a
    HiddenField :: TextFieldOptions a -> Field a
    EnumField   :: EnumFieldOptions a -> Field a

-------------------------------------------------------------------------------
-- TextField
-------------------------------------------------------------------------------

data TextFieldOptions a = TextFieldOptions
    { tfoName   :: FieldName
    , tfoEncode :: a -> Text
    , tfoDecode :: Text -> Either Text a
    }

instance HasFieldName (TextFieldOptions a) where
    fieldName = lens tfoName $ \s x -> s { tfoName = x }

-------------------------------------------------------------------------------
-- EnumField
-------------------------------------------------------------------------------

data EnumFieldOptions a = EnumFieldOptions
    { efoName   :: FieldName
    , efoEncode :: a -> Text
    , efoDecode :: Text -> Either Text a
    , efoValues :: [a]  -- TODO: change to Either Link [a]
    , efoToHtml :: a -> Html ()
    }

instance HasFieldName (EnumFieldOptions a) where
    fieldName = lens efoName $ \s x -> s { efoName = x }

-------------------------------------------------------------------------------
-- Field smart constructors
-------------------------------------------------------------------------------

textField
    :: (ToHttpApiData a, FromHttpApiData a)
    => FieldName -> Field a
textField n = TextField TextFieldOptions
    { tfoName   = n
    , tfoEncode = toQueryParam
    , tfoDecode = parseQueryParam
    }

hiddenField
    :: (ToHttpApiData a, FromHttpApiData a)
    => FieldName -> Field a
hiddenField n = HiddenField TextFieldOptions
    { tfoName   = n
    , tfoEncode = toQueryParam
    , tfoDecode = parseQueryParam
    }

enumField
    :: (Enum a, Bounded a, ToHttpApiData a, FromHttpApiData a, ToHtml a)
    => FieldName -> Field a
enumField n = EnumField EnumFieldOptions
    { efoName   = n
    , efoEncode = toQueryParam
    , efoDecode = parseQueryParam
    , efoValues = [ minBound .. maxBound ]
    , efoToHtml = toHtml
    }

-------------------------------------------------------------------------------
-- Markup
-------------------------------------------------------------------------------

data FormOptions = FormOptions
    { foName :: !Text
    , foUrl  :: !Link
    }
  deriving (Show)

lomakeHtml'
    :: forall xs m. Monad m
    => FormOptions
    -> NP Field xs
    -> NP Maybe xs  -- ^ values
    -> HtmlT m ()
lomakeHtml' formOpts fields values =
    row_ formAttributes $ large_ 12 $ do
        -- inputs
        evalStateT (go fields values) mempty

        -- buttons
        row_ $ large_ 12 $ div_ [ class_ "button-group" ] $ do
            button_ [ class_ "button success", data_ "lomake-action" "submit", disabled_ "disabled" ] "Submit"
            button_ [ class_ "button", data_ "lomake-action" "reset", disabled_ "disabled" ] "Reset"
  where
    formAttributes =
        [ data_ "lomake-form" $ foName formOpts
        , data_ "lomake-form-submit" $ toUrlPiece $ foUrl formOpts
        ]

    go :: NP Field ys -> NP Maybe ys -> StateT (Set Text) (HtmlT m) ()
    go (f :* fs) (x :* xs) = render f x >> go fs xs
    go Nil Nil             = pure ()
#if __GLASGOW_HASKELL__ < 800
    go _ _                 = error "panic"
#endif

    render :: forall a. Field a -> Maybe a -> StateT (Set Text) (HtmlT m) ()
    render (TextField opts) value = do
        n <- inputName opts
        lift $ row_ $ large_ 12 $ label_ $ do
            toHtml (opts ^. fieldName) -- TODO: use label?
            input_
                [ data_ "lomake-id" n
                , name_ n
                , type_ "text"
                , value_ $ maybe "" (tfoEncode opts) value
                ]

    render (HiddenField opts) value = do
        n <- inputName opts
        lift $ input_
            [ data_ "lomake-id" n
            , name_ n
            , type_ "hidden"
            , value_ $ maybe "" (tfoEncode opts) value
            ]

    render (EnumField opts) value = do
        let p = efoEncode opts
        n <- inputName opts
        lift $ row_ $ large_ 12 $ label_ $ do
            toHtml (opts ^. fieldName) -- TODO: use label?
            select_ [ data_ "lomake-id" n, name_ n ] $ do
                when (isNothing value) $
                    optionSelected_ True [] "-"

                for_ (efoValues opts) $ \v ->
                    optionSelected_ (fmap p value == Just (p v))
                        [ value_ $ p v ]
                        (hoist (return . runIdentity) $ efoToHtml opts v)

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

inputName :: (MonadState (Set Text) n, HasFieldName a) => a -> n Text
inputName a = do
    _used <- use id
    return n1
  where
    n0 = a ^. fieldName
    -- n1 = T.map f (T.toLower n0)
    n1 = T.map f n0

    f c | isAlphaNum c = c
        | otherwise    = '-'

-------------------------------------------------------------------------------
-- FromJSON
-------------------------------------------------------------------------------

lomakeParseJSON
    :: SOP.IsProductType a xs
    => NP Field xs
    -> Value
    -> Aeson.Parser a
lomakeParseJSON fs value = parseJSON value >>= \fields ->
    evalStateT (lomakeParseJSON' fields fs) mempty

lomakeParseJSON'
    :: forall a xs m. (SOP.IsProductType a xs, MonadFail m)
    => HashMap Text Text
    -> NP Field xs
    -> StateT (Set Text) m a
lomakeParseJSON' hm =
    fmap (SOP.to . SOP.SOP . SOP.Z) . SOP.hsequence . SOP.hmap parse
  where
    parse :: Field x -> StateT (Set Text) m x
    parse (HiddenField opts) = do
        n <- inputName opts
        lookupField n (tfoDecode opts)
    parse (TextField opts) = do
        n <- inputName opts
        lookupField n (tfoDecode opts)
    parse (EnumField opts) = do
        n <- inputName opts
        lookupField n (efoDecode opts)

    lookupField :: FieldName -> (Text -> Either Text x) -> StateT (Set Text) m x
    lookupField n decoder = case HM.lookup n hm of
        Nothing -> lift $ fail $ "Missing field " ++ n ^. unpacked
        Just t  -> either (fail . errorFormat n) pure (decoder t)

    errorFormat :: FieldName -> Text -> String
    errorFormat n err = "field " ++ n ^. unpacked ++ ": " ++ err ^. unpacked

-------------------------------------------------------------------------------
-- Lomake Request
-------------------------------------------------------------------------------

class SOP.IsProductType a (LomakeCode a) => HasLomake a where
    type LomakeCode a :: [*]
    type LomakeCode a = UnSingleton (SOP.Code a)

    lomake :: Proxy a -> NP Field (LomakeCode a)

-- | A newtype to allow parsing different 'Lomake'
newtype LomakeRequest a = LomakeRequest { getLomakeRequest :: a }

instance HasLomake a => FromJSON (LomakeRequest a) where
    parseJSON = fmap LomakeRequest . lomakeParseJSON (lomake (Proxy :: Proxy a))

-- | TODO: HasLomake should require something?
instance ToSchema (LomakeRequest a) where
    declareNamedSchema _ = pure $ NamedSchema (Just "Lomake") mempty

-------------------------------------------------------------------------------
-- Lomake Response
-------------------------------------------------------------------------------

data LomakeResponse
    = LomakeResponseNoop            -- ^ Do nothing
    | LomakeResponseRedirect !Text  -- ^ redirect to the url
  deriving (Eq, Ord, Show, Typeable, Generic)

instance ToJSON LomakeResponse
instance FromJSON LomakeResponse
instance ToSchema LomakeResponse
