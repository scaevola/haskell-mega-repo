module Futurice.App.FUM.Types (
    module FUM.Types.Login,
    module FUM.Types.GroupName,
    module Futurice.Email,
    module Futurice.App.FUM.Types.Customer,
    module Futurice.App.FUM.Types.Employee,
    module Futurice.App.FUM.Types.Group,
    module Futurice.App.FUM.Types.GroupMatch,
    module Futurice.App.FUM.Types.GroupType,
    module Futurice.App.FUM.Types.Identifier,
    module Futurice.App.FUM.Types.Mailbox,
    module Futurice.App.FUM.Types.Password,
    module Futurice.App.FUM.Types.Status,
    module Futurice.App.FUM.Types.UnixID,
    module Futurice.App.FUM.Types.World,
    AuthUser (..),
    Rights (..),
    hasITRights,
    hasRights,
    ) where

import Futurice.Prelude
import Prelude ()

import Futurice.Email
import FUM.Types.Login
import FUM.Types.GroupName

import Futurice.App.FUM.Types.Customer
import Futurice.App.FUM.Types.Employee
import Futurice.App.FUM.Types.Group
import Futurice.App.FUM.Types.GroupMatch
import Futurice.App.FUM.Types.GroupType
import Futurice.App.FUM.Types.Identifier
import Futurice.App.FUM.Types.Mailbox
import Futurice.App.FUM.Types.Password
import Futurice.App.FUM.Types.Status
import Futurice.App.FUM.Types.UnixID
import Futurice.App.FUM.Types.World

data Rights = RightsOther | RightsNormal | RightsIT
  deriving (Eq, Ord, Show)

-- | Authorised user.
data AuthUser = AuthUser
    { authLogin  :: !Login
    , authRights :: !Rights
    }
  deriving (Eq, Ord, Show)

hasITRights :: AuthUser -> Bool
hasITRights = (RightsIT ==) . authRights

-- | Has IT or Admin
hasRights :: AuthUser -> Bool
hasRights = (RightsOther /=) . authRights
