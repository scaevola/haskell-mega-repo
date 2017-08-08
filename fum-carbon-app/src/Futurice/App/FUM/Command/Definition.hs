{-# LANGUAGE CPP                     #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE KindSignatures          #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE UndecidableSuperClasses #-}
#endif
module Futurice.App.FUM.Command.Definition where

import Futurice.Generics
import Futurice.Lomake
import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.Types

-- | Commands are parameterised by command 'Phase'.
--
-- * User submits 'Input' commands
--
-- * which are amended to the 'Internal' commands, which are stored in persistence layer.
--
-- /Note:/ For some commands there aren't difference between phases!
--
data Phase = Input | Internal

-- | Class describing different commands.
class
    ( HasLomake (cmd 'Input)
    , FromJSON (cmd 'Internal)
    , ToJSON (cmd 'Internal)
    )
    => Command (cmd :: Phase -> *)
  where
    -- | Command tag. Used to distinguish commands in command union.
    commandTag :: Proxy cmd -> Text

    -- | Transform command from 'Input' to 'Internal' phase.
    internalizeCommand
        :: World                              -- ^ world is useful so we can fail early
        -> cmd 'Input                         -- ^ input command
        -> IO (Either String (cmd 'Internal)) -- ^ we can do IO and fail.

    -- | Apply 'Internal' command on the 'World'.
    --
    -- /Note:/ we can fail in this phase too.
    -- However fail conditions should be rare and treated as fatal.
    --
    applyCommand
        :: cmd 'Internal
        -> World
        -> Either String (LomakeResponse, World)
