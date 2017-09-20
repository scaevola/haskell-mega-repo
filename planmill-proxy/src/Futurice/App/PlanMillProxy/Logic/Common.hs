{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -freduction-depth=0 #-}
#endif
module Futurice.App.PlanMillProxy.Logic.Common (
    HasPostgresPool,
    poolExecuteMany,
    module Futurice.App.PlanMillProxy.Logic.Common
    ) where

import Control.Monad.Catch        (handle)
import Data.Aeson.Compat          (FromJSON)
import Data.Binary.Get            (Get, runGetOrFail)
import Data.Binary.Tagged
       (HasSemanticVersion, HasStructuralInfo, SemanticVersion, Version,
       structuralInfo, structuralInfoSha1ByteStringDigest)
import Data.Constraint
import Futurice.Metrics.RateMeter (mark)
import Futurice.PostgresPool
import Futurice.Prelude
import Futurice.Servant           (CachePolicy (..), genCachedIO)
import GHC.TypeLits               (natVal)
import Prelude ()

import PlanMill.Types.Query (Query (..), queryDict, queryToRequest)
import PlanMill.Worker      (submitPlanMill)

import qualified Database.PostgreSQL.Simple as Postgres

import Futurice.App.PlanMillProxy.Types (Ctx (..))

-------------------------------------------------------------------------------
-- Intervals
-------------------------------------------------------------------------------

genericAge :: String
genericAge = "'6 hours'"

capacityAge :: String
capacityAge = "'12 hours'"

-- Timereports are updated by updating oldest ones, so no age guarantee
-- atm

-------------------------------------------------------------------------------
-- LIO
-------------------------------------------------------------------------------

type LIO = LogT IO

runLIO :: Ctx -> LIO a -> IO a
runLIO =  runLogT'

-------------------------------------------------------------------------------
-- Utiltities
-------------------------------------------------------------------------------

-- | Run query on real planmill backend.
fetchFromPlanMill :: Ctx -> Query a -> LIO a
fetchFromPlanMill ctx q = case (typeableDict, fromJsonDict, nfdataDict) of
    (Dict, Dict, Dict) -> liftIO $
        genCachedIO RequestNew logger cache (10 * 60) q $
            submitPlanMill ws (queryToRequest q)
  where
    typeableDict = queryDict (Proxy :: Proxy Typeable) q
    fromJsonDict = queryDict (Proxy :: Proxy FromJSON) q
    nfdataDict   = queryDict (Proxy :: Proxy NFData) q

    ws     = ctxWorkers ctx
    logger = ctxLogger ctx
    cache  = ctxCache ctx

handleSqlError :: Postgres.Query -> a -> IO a -> LIO a
handleSqlError q x action = handle (omitSqlError q x) $ liftIO action

omitSqlError :: Postgres.Query -> a -> Postgres.SqlError -> LIO a
omitSqlError q a err = do
    liftIO $ mark "Omitted sql error"
    logAttention (textShow err) (show q)
    return a

runLogT' :: Ctx -> LogT IO a -> IO a
runLogT' ctx = runLogT "logic" (ctxLogger ctx)

-------------------------------------------------------------------------------
-- Safe pool stuff
-------------------------------------------------------------------------------

safePoolQuery
    :: (Postgres.ToRow q, Postgres.FromRow r, HasPostgresPool ctx)
    => ctx -> Postgres.Query -> q -> LIO [r]
safePoolQuery ctx query row = handleSqlError query [] $
    poolQuery ctx query row

safePoolQuery_
    :: (Postgres.FromRow r, HasPostgresPool ctx)
    => ctx -> Postgres.Query -> LIO [r]
safePoolQuery_ ctx query = handleSqlError query [] $
    poolQuery_ ctx query

safePoolExecute
    :: (Postgres.ToRow q, HasPostgresPool ctx)
    => ctx -> Postgres.Query -> q -> LIO Int64
safePoolExecute ctx query row = handleSqlError query 0 $
    poolExecute ctx query row

safePoolExecute_
    :: (HasPostgresPool ctx)
    => ctx -> Postgres.Query -> LIO Int64
safePoolExecute_ ctx query = handleSqlError query 0 $ 
    poolExecute_ ctx query

safePoolExecuteMany
    :: (Postgres.ToRow q, HasPostgresPool ctx)
    => ctx -> Postgres.Query -> [q] -> LIO Int64
safePoolExecuteMany ctx query rows = handleSqlError query 0 $
    poolExecuteMany ctx query rows

-------------------------------------------------------------------------------
-- binary-tagged additions
-------------------------------------------------------------------------------

-- | Check whether the tag at the beginning of the 'LazyByteString' is correct.
checkTagged
    :: forall a. (HasStructuralInfo a, HasSemanticVersion a)
    => Proxy a -> LazyByteString -> Bool
checkTagged _ lbs = either (const False) (view _3) $ runGetOrFail decoder lbs
  where
    decoder :: Get Bool
    decoder = do
        ver <- get
        hash' <- get
        pure $ ver == ver' && hash' == hash''

    proxyV = Proxy :: Proxy (SemanticVersion a)
    proxyA = Proxy :: Proxy a
    ver' = fromIntegral (natVal proxyV) :: Version
    hash'' = structuralInfoSha1ByteStringDigest . structuralInfo $ proxyA
