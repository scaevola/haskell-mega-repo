{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeFamilies    #-}
-- | Different monad classes
--
-- /TODO:/ add error handling to classes
module Futurice.Integrations.Classes (
    MonadFUM(..),
    MonadGitHub(..),
    MonadFlowdock(..),
    ) where

import Futurice.Prelude
import Prelude ()

import Control.Monad.FUM (MonadFUM (..))
import Data.Constraint   (Constraint)

import qualified Chat.Flowdock.REST as FD
import qualified GitHub             as GH

class Monad m => MonadGitHub m where
    type MonadGitHubC m a :: Constraint
    type MonadGitHubC m a = ()

    githubReq :: MonadGitHubC m a => GH.Request 'GH.RA a -> m a

class Monad m => MonadFlowdock m where
    flowdockOrganisationReq :: FD.ParamName FD.Organisation -> m FD.Organisation
