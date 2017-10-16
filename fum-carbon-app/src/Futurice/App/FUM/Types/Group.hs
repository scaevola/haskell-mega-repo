{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.FUM.Types.Group where

import FUM.Types.GroupName
import FUM.Types.Login
import Futurice.Email
import Futurice.IdMap      (HasKey (..))
import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.Types.GroupType
import Futurice.App.FUM.Types.Identifier
import Futurice.App.FUM.Types.UnixID
import Futurice.App.FUM.Types.GroupMatch

-- | Group: email list or access group.
data Group = Group
    { _groupName         :: !GroupName
    , _groupGID          :: !GID
    , _groupType         :: !GroupType
    , _groupDescription  :: !Text
    , _groupEmailAliases :: ![Email]
    , _groupMatch        :: !GroupMatch
    -- Graph wiring:
    , _groupEditor       :: !(Set GroupName)  -- ^ if 'null', editors are members of the group
    , _groupEmployees    :: !(Set Login)
    , _groupCustomers    :: !(Set Login)
    }
  deriving (Eq, Ord, Show, Typeable, Generic)

makeLenses ''Group
deriveGeneric ''Group

instance NFData Group

instance HasKey Group where
    type Key Group = GroupName
    key = groupName

instance Entity Group where entityName _ = "FUM.Group"
