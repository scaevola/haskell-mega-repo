{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.FUM.Types.Mailbox where

import Futurice.Email
import Futurice.IdMap      (HasKey (..))
import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.Types.Identifier

-- | Shared mailbox.
data Mailbox = Mailbox
    { _mailboxId           :: !(Identifier Mailbox)
    , _mailboxEmail        :: !Email
    , _mailboxEmailAliases :: ![Email]
    }
  deriving (Eq, Ord, Show, Typeable, Generic)

makeLenses ''Mailbox
deriveGeneric ''Mailbox

instance NFData Mailbox

instance HasKey Mailbox where
    type Key Mailbox = Identifier Mailbox
    key = mailboxId

instance Entity Mailbox   where entityName _ = "FUM.Mailbox"
