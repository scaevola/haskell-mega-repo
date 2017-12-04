{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.PlanMillSync.Actions where

import FUM.Types.Login  (Login, loginRegexp)
import Text.Regex.Applicative.Text (match)
import Futurice.Prelude
import Prelude ()
import Data.List (find)
import Servant (Handler, ServantErr (..), err400, err500)

import qualified Personio as P
import qualified PlanMill as PM
import qualified PlanMill.Worker             as PM
import qualified PlanMill.Queries as PMQ

import Futurice.App.PlanMillSync.Ctx
import Futurice.App.PlanMillSync.Monad

-- | Return date for update
canUpdateDepartDate :: P.Employee -> PM.User -> Maybe Day
canUpdateDepartDate p pmu
    | pEnd /= pmEnd = pEnd
    | otherwise     = Nothing
  where
    pEnd = p ^. P.employeeEndDate
    pmEnd = PM.uDepartDate pmu

updateDepartDate :: Ctx -> Login -> Handler Text
updateDepartDate ctx login = runLogT "update-depart-date" lgr $ do
    logInfo "Add depart date" login
    (p, pm) <- liftIO (fetchUser ctx login) >>= either throwError pure
    case canUpdateDepartDate p pm of
        Nothing -> throwError err400 { errBody = "Not updateable depart date" }
        Just d  -> do
            let pm' = pm { PM.uDepartDate = Just d }
            logInfo "Editing PlanMill user" pm'

            -- Write to PM.
            let req = PM.editUser pm'
            res <- liftIO $ for (ctxWorkers ctx) $ \workers ->
                PM.submitPlanMillE workers req

            case res of
                Nothing         -> pure "No workers, done nothing"
                Just (Left err) -> do
                    logAttention "Failed PlanMill update" (show err)
                    throwError err500 { errBody = fromString $ show err } -- TODO: proper encoding
                Just (Right ()) -> pure $ textShow (d, p, pm, pm')
  where
    lgr = ctxLogger ctx

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------

-- | Note: maybe we want to return PMUser (from .Types)
fetchUser :: Ctx -> Login -> IO (Either ServantErr (P.Employee, PM.User))
fetchUser ctx login = runIntegrations' ctx $ do
    ps <- P.personio P.PersonioEmployees

    case find (\p -> p ^. P.employeeLogin == Just login) ps of
        Nothing -> pure $ Left err400 { errBody = "Cannot find personio user" }
        Just p  -> do
            pms <- toList <$> PMQ.users
            case find (\pm -> pmLogin pm == Just login) pms of
                Nothing -> pure $ Left err400 { errBody = "Cannot find planmill user" }
                Just u  -> pure $ Right (p, u)

pmLogin :: PM.User -> Maybe Login
pmLogin u = match loginRe (PM.uUserName u)
  where
    loginRe = "https://login.futurice.com/openid/" *> loginRegexp
