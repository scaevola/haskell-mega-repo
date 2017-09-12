{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Futurice.App.Proxy (
    defaultMain,
    ) where

import Data.Aeson.Compat               (object, (.=))
import Data.Maybe                      (isNothing)
import Data.Pool                       (createPool, withResource)
import Data.Reflection                 (Given (..), give)
import Data.Text.Encoding              (decodeLatin1)
import Futurice.Prelude
import Futurice.Servant
import Network.Wai                     (Request, rawPathInfo)
import Network.Wai.Middleware.HttpAuth (basicAuth')
import Prelude ()
import Servant
import Servant.Binary.Tagged           (BINARYTAGGED)
import Servant.Client
import Servant.Common.Req              (Req (headers))
import Servant.Proxy
import Text.Regex.Applicative.Text     (RE', anySym, match, string)

import qualified Data.Text                  as T
import qualified Database.PostgreSQL.Simple as Postgres
import qualified FUM
import qualified Futurice.GitHub            as GH (SomeRequest, SomeResponse)
import qualified Personio
import qualified PlanMill.Types.Query       as PM (SomeQuery, SomeResponse)

import Futurice.App.Proxy.Config
import Futurice.App.Proxy.Ctx
import Futurice.App.Reports.MissingHours
       (MissingHoursReport, MissingHoursTitle)
import Futurice.App.Reports.TimereportsByTask (TimereportsByTaskReport)

-------------------------------------------------------------------------------
-- Services
-------------------------------------------------------------------------------

data ReportsAppService
data PlanmillProxyService
data GithubProxyService
data FumService
data PowerService
data PersonioProxyService

instance HasClientBaseurl Ctx ReportsAppService where
    clientBaseurl _ = lens ctxReportsAppBaseurl $ \ctx x ->
        ctx { ctxReportsAppBaseurl = x }

instance HasClientBaseurl Ctx PlanmillProxyService where
    clientBaseurl _ = lens ctxPlanmillProxyBaseurl $ \ctx x ->
        ctx { ctxPlanmillProxyBaseurl = x }

instance HasClientBaseurl Ctx GithubProxyService where
    clientBaseurl _ = lens ctxGithubProxyBaseurl $ \ctx x ->
        ctx { ctxGithubProxyBaseurl = x }

instance HasClientBaseurl Ctx FumService where
    clientBaseurl _ = lens ctxFumBaseurl $ \ctx x ->
        ctx { ctxFumBaseurl = x }

instance HasClientBaseurl Ctx PowerService where
    clientBaseurl _ = lens ctxPowerBaseurl $ \ctx x ->
        ctx { ctxPowerBaseurl = x }

instance HasClientBaseurl Ctx PersonioProxyService where
    clientBaseurl _ = lens ctxPersonioProxyBaseurl $ \ctx x ->
        ctx { ctxPersonioProxyBaseurl = x }

-------------------------------------------------------------------------------
-- Endpoints
-------------------------------------------------------------------------------

-- Reports
type MissingReportsEndpoint = ProxyPair
    ("futuhours" :> "reports" :> "missinghours" :> Get '[CSV, JSON] (MissingHoursReport MissingHoursTitle))
    ReportsAppService
    ("missing-hours" :> Get '[JSON] (MissingHoursReport MissingHoursTitle))

type TimereportsByTaskReportEndpoint = ProxyPair
    ("reports" :> "hours-by-task" :> Get '[CSV, JSON] TimereportsByTaskReport)
    ReportsAppService
    ("hours-by-task" :> Get '[JSON] TimereportsByTaskReport)

-- Planmill
-- TODO: we actually decode/encode when proxying.
-- Is this bad?
type PlanmillProxyEndpoint' =
    ReqBody '[JSON] [PM.SomeQuery] :> Post '[BINARYTAGGED] [Either Text PM.SomeResponse]
type PlanmillProxyEndpoint = ProxyPair
    ("planmill-haxl" :> PlanmillProxyEndpoint')
    PlanmillProxyService
    ("planmill-haxl" :> PlanmillProxyEndpoint')

-- Github
type GithubProxyEndpoint' =
    ReqBody '[JSON] [GH.SomeRequest] :> Post '[BINARYTAGGED] [Either Text GH.SomeResponse]

type GithubProxyEndpoint = ProxyPair
    ("github-haxl" :> GithubProxyEndpoint')
    GithubProxyService
    ("github-haxl" :> GithubProxyEndpoint')

-- Fum
type FumEmployeesEndpoint = ProxyPair
    ("fum" :> "list" :> "employees" :> Get '[JSON] Value)
    FumService
    ("list" :> "employees" :> Get '[JSON] Value)

type FumUserEndpoint = ProxyPair
    ("fum" :> "users" :> Capture "uid" Text :> Get '[JSON] Value)
    FumService
    (WithFumAuthToken :> "users" :> Capture "uid" Text :> Get '[JSON] Value)

-- Power
type PowerBiEndpoint = ProxyPair
    ("power" :> "api" :> "power_bi" :> QueryParam "month" Text :> QueryParam "start_month" Text :> QueryParam "end_month" Text :> QueryParam "limit" Int :> QueryParam "span" Int :> QueryParam "tribe" Text :> Get '[JSON] Value)
    PowerService
    ("api" :> "v2" :> "power_bi" :> QueryParam "month" Text :> QueryParam "start_month" Text :> QueryParam "end_month" Text :> QueryParam "limit" Int :> QueryParam "span" Int:> QueryParam "tribe" Text :> Get '[JSON] Value)

type PowerCompetencesEndpoint = ProxyPair
    ("power" :> "api" :> "company_competences" :> Get '[JSON] Value)
    PowerService
    ("api" :> "v2" :> "company_competences" :> Get '[JSON] Value)

-- Personio via FUM
type PersonioProxyEndpoint' =
    ReqBody '[JSON] Personio.SomePersonioReq :>
    Post '[JSON] Personio.SomePersonioRes

type PersonioProxyEndpoint = ProxyPair
    ("personio-request" :> PersonioProxyEndpoint')
    PersonioProxyService
    ("api" :> "personio-request" :> PersonioProxyEndpoint')

-- | Whole proxy definition
type ProxyDefinition =
    '[ MissingReportsEndpoint
    , TimereportsByTaskReportEndpoint
    , PlanmillProxyEndpoint
    , GithubProxyEndpoint
    , PersonioProxyEndpoint
    , FumEmployeesEndpoint
    , FumUserEndpoint
    , PowerBiEndpoint
    , PowerCompetencesEndpoint
    ]

type ProxyAPI  = Get '[JSON] Text :<|> ProxyServer ProxyDefinition

proxyAPI :: Proxy ProxyAPI
proxyAPI = Proxy

-------------------------------------------------------------------------------
-- Fum AuthToken "hack"
-------------------------------------------------------------------------------

-- We use @reflection@ to 'give' 'FUM.AuthToken'.

data WithFumAuthToken
instance (Given FUM.AuthToken, HasClient api)
    => HasClient (WithFumAuthToken :> api)
  where
    type Client (WithFumAuthToken :> api) = Client api
    clientWithRoute _ req = clientWithRoute (Proxy :: Proxy api) req'
      where
        req' = req { headers = ("Authorization", "Token " <> given ^. FUM.getAuthToken) : headers req }

-------------------------------------------------------------------------------
-- WAI/startup
------------------------------------------------------------------------------

server :: Ctx -> Server ProxyAPI
server ctx = give (ctxFumAuthToken ctx) $ pure "P-R-O-X-Y"
    :<|> makeProxy (Proxy :: Proxy MissingReportsEndpoint) ctx
    :<|> makeProxy (Proxy :: Proxy TimereportsByTaskReportEndpoint) ctx
    :<|> makeProxy (Proxy :: Proxy PlanmillProxyEndpoint) ctx
    :<|> makeProxy (Proxy :: Proxy GithubProxyEndpoint) ctx
    :<|> makeProxy (Proxy :: Proxy PersonioProxyEndpoint) ctx
    :<|> makeProxy (Proxy :: Proxy FumEmployeesEndpoint) ctx
    :<|> makeProxy (Proxy :: Proxy FumUserEndpoint) ctx
    :<|> makeProxy (Proxy :: Proxy PowerBiEndpoint) ctx
    :<|> makeProxy (Proxy :: Proxy PowerCompetencesEndpoint) ctx

defaultMain :: IO ()
defaultMain = futuriceServerMain makeCtx $ emptyServerConfig
    & serverName         .~ "Proxy-app"
    & serverDescription  .~ "Proxy from the outer space"
    & serverColour       .~ (Proxy :: Proxy ('FutuAccent 'AF3 'AC3))
    & serverApp proxyAPI .~ server
    & serverMiddleware   .~ (\ctx -> basicAuth' (checkCreds ctx) "P-R-O-X-Y")
    & serverEnvPfx       .~ "PROXYMGMT"
  where
    makeCtx :: Config -> Logger -> DynMapCache -> IO (Ctx, [Job])
    makeCtx Config {..} logger _cache = do
        mgr                  <- newManager tlsManagerSettings
        postgresPool         <- createPool
            (Postgres.connect cfgPostgresConnInfo)
            Postgres.close
            1 10 5
        pure $ flip (,) [] Ctx
            { ctxManager              = mgr
            , ctxPostgresPool         = postgresPool
            , ctxReportsAppBaseurl    = cfgReportsAppBaseurl
            , ctxPlanmillProxyBaseurl = cfgPlanmillProxyBaseurl
            , ctxGithubProxyBaseurl   = cfgGithubProxyBaseurl
            , ctxPersonioProxyBaseurl = cfgPersonioProxyBaseurl
            , ctxFumBaseurl           = cfgFumBaseurl
            , ctxFumAuthToken         = cfgFumAuthToken
            , ctxPowerBaseurl         = cfgPowerBaseurl
            , ctxLogger               = logger
            }

checkCreds :: Ctx -> Request -> ByteString -> ByteString -> IO Bool
checkCreds ctx req u p = withResource (ctxPostgresPool ctx) $ \conn -> do
    let u' = decodeLatin1 u
        p' = decodeLatin1 p
        endpoint = decodeLatin1 $ rawPathInfo req
    case match isSwaggerReg endpoint of
        Nothing -> regularCheck conn u' p' endpoint
        Just _  -> swaggerCheck conn u' p' endpoint
  where
    regularCheck :: Postgres.Connection -> Text -> Text -> Text -> IO Bool
    regularCheck conn u' p' endpoint = do
        res <- Postgres.query conn (fromString credentialAndEndpointCheck)
            (u', p', endpoint) :: IO [Postgres.Only Int]
        case res of
            []    -> logInvalidLogin u' endpoint
            _ : _ -> do
                _ <- logAccess conn u' endpoint
                pure True

    swaggerCheck :: Postgres.Connection -> Text -> Text -> Text -> IO Bool
    swaggerCheck conn u' p' endpoint = do
        res <- Postgres.query conn (fromString credentialCheck)
            (u', p') :: IO [Postgres.Only Int]
        case res of
            []    -> logInvalidLogin u' endpoint
            _ : _ -> pure True

    logInvalidLogin :: Text -> Text -> IO Bool
    logInvalidLogin u' endpoint = runLogT "checkCreds" (ctxLogger ctx) $ do
        logAttention "Invalid login with " $ object
            [ "username" .= u'
            , "endpoint" .= endpoint
            ]
        pure False

    -- | Logs user, and requested endpoint if endpoint is not swagger-related.
    logAccess :: Postgres.Connection -> Text -> Text -> IO ()
    logAccess conn user endp =
        when (isNothing $ match isSwaggerReg endp) $ void $
            Postgres.execute conn
            "insert into proxyapp.accesslog (username, endpoint) values (?, ?);"
            (user, endp)

    isSwaggerReg :: RE' Text
    isSwaggerReg = string "/swagger.json" <|> string "/swagger-ui" *> (T.pack <$> many anySym)

    credentialCheck :: String
    credentialCheck = unwords
        [ "SELECT 1 FROM proxyapp.credentials"
        , "WHERE username = ? AND passtext = crypt(?, passtext)"
        , ";"
        ]

    credentialAndEndpointCheck :: String
    credentialAndEndpointCheck = init credentialCheck
        ++ " AND ? LIKE endpoint || '%';"
