{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}

module Dashdo.Servant where

import Dashdo
import Dashdo.Files
import Dashdo.Types

import Control.Exception.Safe (catchAnyDeep)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid            ((<>))
import Data.Text.Lazy         (Text)
import Lucid
import Network.Wai            (Application)
import Servant
import Servant.HTML.Lucid     (HTML)
import System.Random          (randomIO)

import qualified Data.Map                as Map
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.UUID.Types         as UUID

type RunInIO m = forall a. m a -> IO a

getRandomUUID :: IO UUID.UUID
getRandomUUID = randomIO

data JavaScript

instance Accept JavaScript where
    contentType _ = "application/javascript"

instance MimeRender JavaScript Text where
    mimeRender _ = TLE.encodeUtf8

type P = (Text, Text)

-- Note: we use Text for JS because ByteString doesn't have
-- swagger schema
type DashdoAPI = Get '[HTML] (Html ())
    :<|> "uuid" :> Get '[JSON] UUID.UUID
    :<|> "js" :> "dashdo.js" :> Get '[JavaScript] Text
    :<|> "js" :> "runners" :> "rdashdo.js" :> Get '[JavaScript] Text
    :<|> Capture "handler" T.Text :> ReqBody '[FormUrlEncoded] [P] :> Post '[HTML] (Html ())

dashdoServer :: forall m. Monad m => RunInIO m -> Text -> [RDashdo m] -> IO (Server DashdoAPI)
dashdoServer r iniHtml ds = do
    uuid <- getRandomUUID
    pure $ pure (toHtmlRaw iniHtml)
        :<|> pure uuid
        :<|> pure (TLE.decodeUtf8 dashdoJS)
        :<|> pure (TLE.decodeUtf8 dashdoJSrunnerRdash)
        :<|> handler
  where
    handler :: T.Text -> [P] -> Handler (Html ())
    handler n f = case Map.lookup n hs of
        Nothing -> throwError err404
        Just h  -> liftIO $ h >>= ($ f)

    hs :: Map.Map T.Text (IO ([P] -> IO (Html ())))
    hs = Map.fromList $ map (\(RDashdo n _ d) -> (T.pack n, makeHandler d)) ds
      where
        makeHandler :: Dashdo m a -> IO ([P] -> IO (Html ()))
        makeHandler d = do
            (_iniHtml, ff, _) <- r $ dashdoGenOut d (initial d) []
            pure $ \ps -> do
                let newVal = parseForm (initial d) ff ps
                (thisHtml, _, _) <- liftIO $ (r $ dashdoGenOut d newVal ps) `catchAnyDeep` \e -> do
                    let es :: String
                        es = "<div  class=\"alert alert-danger\" role=\"alert\"><pre>Error: " <> show e <> "</pre></div>"

                        errorHtml = TL.pack es <> iniHtml
                    pure (errorHtml, [], [])
                pure $ toHtmlRaw thisHtml

dashdoApplication :: Monad m => RunInIO m -> Text -> [RDashdo m] -> IO Application
dashdoApplication r iniHtml ds = do
    server <- dashdoServer r iniHtml ds
    pure $ serve (Proxy :: Proxy DashdoAPI) server
