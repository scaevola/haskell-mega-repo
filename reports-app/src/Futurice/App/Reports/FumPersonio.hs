{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
#endif
-- | FUM
module Futurice.App.Reports.FumPersonio (
    -- * Report
    FumPersonioReport,
    fumPersonioReport,
    -- * Types
    PersonioUser (..),
    FUMUser (..),
    ) where

import Prelude ()
import Futurice.Prelude
import Futurice.Generics

import Control.Arrow             ((&&&))
import Futurice.Integrations
import Futurice.Report.Columns

import qualified Data.HashMap.Strict as HM
import qualified Data.Vector         as V
import qualified FUM
import qualified Personio

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data PersonioUser = PersonioUser
    { _personioUserName  :: !Text
    , _personioUserLogin :: !FUM.Login
    }
    deriving (Eq, Ord, Show, Typeable, Generic)

data FUMUser = FUMUser
    { _fumUserName  :: !Text
    , _fumUserLogin :: !FUM.Login
    }
    deriving (Eq, Ord, Show, Typeable, Generic)

deriveGeneric ''PersonioUser
deriveGeneric ''FUMUser

instance NFData PersonioUser
instance NFData FUMUser

instance ToJSON PersonioUser where
    toJSON = sopToJSON
    toEncoding = sopToEncoding

instance ToJSON FUMUser where
    toJSON = sopToJSON
    toEncoding = sopToEncoding

instance ToSchema PersonioUser where
    declareNamedSchema = sopDeclareNamedSchema
instance ToSchema FUMUser where
    declareNamedSchema = sopDeclareNamedSchema

instance ToColumns PersonioUser
instance ToColumns FUMUser

-------------------------------------------------------------------------------
-- Report
-------------------------------------------------------------------------------

type FumPersonioReport = Report
    "TEMPORARY: Users in (old) FUM ↔ Personio"
    ReportGenerated
    (Vector :$ These PersonioUser FUMUser)

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

fumPersonioReport
    :: forall m env.
       ( MonadTime m, MonadFUM m, MonadPersonio m
       , MonadReader env m
       , HasFUMEmployeeListName env
       )
    => m FumPersonioReport
fumPersonioReport = do
    now <- currentTime
    today <- currentDay
    report <- makeReport <$> personioUsers today <*> fumUsers
    return $ Report (ReportGenerated now) report
  where
    fumUsers :: m [FUMUser]
    fumUsers = map mk . V.toList <$> fumEmployeeList
      where
        mk u = FUMUser
            { _fumUserName  = u ^. FUM.userFullName
            , _fumUserLogin = u ^. FUM.userName
            }

    personioUsers :: Day -> m [PersonioUser]
    personioUsers today = mapMaybe mk <$> Personio.personio Personio.PersonioEmployees
      where
        mk :: Personio.Employee -> Maybe PersonioUser
        mk e = do
            login <- e ^. Personio.employeeLogin
            guard $ maybe True (today <=) $ e ^. Personio.employeeEndDate
            guard $ maybe False (<= today) $ e ^. Personio.employeeHireDate
            pure PersonioUser  
                { _personioUserName  = e ^. Personio.employeeFirst <> " " <> e ^. Personio.employeeLast
                , _personioUserLogin = login
                }

    -- | Align on `
    makeReport
        :: [PersonioUser] -> [FUMUser]
        -> Vector (These PersonioUser FUMUser)
    makeReport ps fs =
        -- Create 'HashMap FUMLogin a' maps, and 'align' them
        let ps' = HM.fromList . map (_personioUserLogin &&& id) $ ps
            fs' = HM.fromList . map (_fumUserLogin &&& id) $ fs
            hm  = align ps' fs'
        in V.fromList . sort . HM.elems $ hm
