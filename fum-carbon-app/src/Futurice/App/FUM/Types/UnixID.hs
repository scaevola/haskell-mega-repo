{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Futurice.App.FUM.Types.UnixID (
    UID,
    GID,
    firstUnixID,
    nextUnixID,
    UnixID,
    getUnixID,
    ) where

import Data.Aeson.Compat (FromJSON (..), ToJSON (..))
import Futurice.Prelude
import Prelude ()

-------------------------------------------------------------------------------
-- Kind
-------------------------------------------------------------------------------

data UserGroup = User | Group
data SUserGroup (t :: UserGroup) where
    SUser  :: SUserGroup 'User
    SGroup :: SUserGroup 'Group
class    IUserGroup (t :: UserGroup) where iusergroup :: SUserGroup t
instance IUserGroup 'User            where iusergroup = SUser
instance IUserGroup 'Group           where iusergroup = SGroup

showConstructor :: forall t. IUserGroup t => Proxy t -> String
showConstructor _ = case iusergroup :: SUserGroup t of
    SUser  -> "UnixID @User"
    SGroup -> "UnixID @SGroup"

-------------------------------------------------------------------------------
-- UnixID
-------------------------------------------------------------------------------

newtype UnixID (t :: UserGroup) = UnixID Word64
  deriving (Eq, Ord)

firstUnixID :: UnixID t
firstUnixID = UnixID 1000

-- | Next UNIX id.
nextUnixID :: UnixID t -> UnixID t
nextUnixID (UnixID x) = UnixID (succ x)

getUnixID :: UnixID t -> Word64
getUnixID (UnixID i) = i

instance IUserGroup t => Show (UnixID t) where
    showsPrec d (UnixID w) = showParen (d > 10)
        $ showString (showConstructor (Proxy @t))
        . showChar ' '
        . showsPrec 11 w
      where

instance ToJSON (UnixID t) where
    toJSON (UnixID w)     = toJSON w
    toEncoding (UnixID w) = toEncoding w

instance FromJSON (UnixID t) where
    parseJSON v = UnixID <$> parseJSON v

instance NFData (UnixID t) where
    rnf (UnixID x) = rnf x

-- instance IUserGroup t => 
-------------------------------------------------------------------------------
-- Aliases
-------------------------------------------------------------------------------

type UID = UnixID 'User
type GID = UnixID 'Group
