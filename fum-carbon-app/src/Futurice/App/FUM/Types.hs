module Futurice.App.FUM.Types (
    module Futurice.App.FUM.Types.Basic,
    module Futurice.App.FUM.Types.GroupType,
    module Futurice.App.FUM.Types.Identifier,
    module Futurice.App.FUM.Types.Status,
    module Futurice.App.FUM.Types.World,
    AuthUser (..),
    Rights (..),
    hasITRights,
    hasRights,
    ) where

import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.Types.Basic
import Futurice.App.FUM.Types.GroupType
import Futurice.App.FUM.Types.Identifier
import Futurice.App.FUM.Types.Status
import Futurice.App.FUM.Types.World

data Rights = RightsOther | RightsNormal | RightsIT
  deriving (Eq, Ord, Show)

-- | Authorised user.
data AuthUser = AuthUser
    { authLogin :: !Login
    , authRights :: !Rights
    }
  deriving (Eq, Ord, Show)

hasITRights :: AuthUser -> Bool
hasITRights = (RightsIT ==) . authRights

-- | Has IT or Admin
hasRights :: AuthUser -> Bool
hasRights = (RightsOther /=) . authRights
