{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.FUM.Types.Customer where

import FUM.Types.Login
import Futurice.IdMap      (HasKey (..))
import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.Types.Identifier
import Futurice.App.FUM.Types.Status
-- import Futurice.App.FUM.Types.UnixID

-- | Any email
type RawEmail = Text

-- | TODO:
type SshKey2 = Text

-- | What can a customer do in Futurice's internal systems.
data CustomerRight
    = CustomerRightWiki   -- ^ access to wikis
    | CustomerRightSSH    -- ^ ssh login to servers
    | CustomerRightEmail  -- ^ can be added to email lists
  deriving (Eq, Ord, Show, Typeable, Generic, Enum, Bounded)

instance NFData CustomerRight

-- | 'Customer's aren't 'Employee's.
--
-- TODO: add UID
data Customer = Customer
    { _customerLogin          :: !Login
    , _customerName           :: !Text
    , _customerSshKeys        :: ![SshKey2]
    , _customerEmail          :: !RawEmail
    , _customerStatus         :: !Status
    , _customerRight          :: !(Set CustomerRight)
    , _employeeActivationDate :: !UTCTime  -- ^ like "hire-date"
    , _employeeSuspendDate    :: !UTCTime  -- ^ there's always to disable
    }
  deriving (Eq, Ord, Show, Typeable, Generic)

makeLenses ''Customer
deriveGeneric ''Customer

instance NFData Customer

instance HasKey Customer where
    type Key Customer = Login
    key = customerLogin

instance Entity Customer  where entityName _ = "FUM.Customer"
