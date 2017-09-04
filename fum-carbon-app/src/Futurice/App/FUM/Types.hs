module Futurice.App.FUM.Types (
    module Futurice.App.FUM.Types.Basic,
    module Futurice.App.FUM.Types.GroupType,
    module Futurice.App.FUM.Types.Identifier,
    module Futurice.App.FUM.Types.Status,
    module Futurice.App.FUM.Types.World,
    AuthUser,
    Rights (..),
    ) where

import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.Types.Basic
import Futurice.App.FUM.Types.GroupType
import Futurice.App.FUM.Types.Identifier
import Futurice.App.FUM.Types.Status
import Futurice.App.FUM.Types.World

data Rights = RightsIT | RightsOther
  deriving (Eq, Ord, Show)

-- | Authorised user.
type AuthUser = (Login, Rights)
