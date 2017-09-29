{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.PlanMillSync.Types where

import Futurice.Prelude
import Prelude ()

import qualified PlanMill         as PM
import qualified PlanMill.Queries as PMQ

data PMUser = PMUser
    { pmUser     :: !PM.User
    , pmTeam     :: !(Maybe PM.Team)
    , pmContract :: !Text
    }
  deriving Show

users :: PMQ.MonadPlanMillQuery m => m [PMUser]
users = do
    us <- toList <$> PMQ.users
    for us $ \u -> do
        u' <- PMQ.user (u ^. PM.identifier)
        t <- traverse PMQ.team (PM.uTeam u')
        c <- PMQ.enumerationValue (PM.uContractType u) "Unknown contract"
        pure PMUser
            { pmUser     = u'
            , pmTeam     = t
            , pmContract = c
            }
