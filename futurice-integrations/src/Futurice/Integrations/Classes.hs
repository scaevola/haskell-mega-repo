{-# LANGUAGE CPP                     #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE TypeFamilies            #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE UndecidableSuperClasses #-}
#endif
-- | Different monad classes
--
-- /TODO:/ add error handling to classes
module Futurice.Integrations.Classes (
    MonadFUM(..),
    MonadFlowdock(..),
    MonadGitHub(..),
    MonadPersonio(..),
    MonadPlanMillQuery(..),
    MonadTime(..),
    ) where

import Control.Monad.FUM      (MonadFUM (..))
import Control.Monad.Personio (MonadPersonio (..))
import Control.Monad.PlanMill (MonadPlanMillQuery (..))
import Data.Constraint        (Constraint)
import Futurice.GitHub        (GHTypes)
import Futurice.Prelude
import Generics.SOP           (All)
import Prelude ()

import qualified Chat.Flowdock.REST as FD
import qualified GitHub             as GH

class (Monad m, All (MonadGitHubC m) GHTypes) => MonadGitHub m where
    type MonadGitHubC m :: * -> Constraint
    githubReq :: MonadGitHubC m a => GH.Request 'GH.RA a -> m a

class Monad m => MonadFlowdock m where
    flowdockOrganisationReq :: FD.ParamName FD.Organisation -> m FD.Organisation
