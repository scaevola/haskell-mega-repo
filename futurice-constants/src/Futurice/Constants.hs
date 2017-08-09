{-# LANGUAGE TemplateHaskell   #-}
module Futurice.Constants (
    fumPublicUrl,
    fumPublicUrlStr,
    personioPublicUrl,
    personioPublicUrlStr,
    ) where

import Futurice.Prelude
import Prelude ()

import qualified Futurice.Constants.Internal as I

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

constants :: I.Constants
constants = $(makeRelativeToProject "constants.json" >>= embedFromJSON (Proxy :: Proxy I.Constants))

-------------------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------------------

fumPublicUrl :: Text
fumPublicUrl = I.fumPublicUrl constants

fumPublicUrlStr :: String
fumPublicUrlStr = fumPublicUrl ^. unpacked

personioPublicUrl :: Text
personioPublicUrl = I.personioPublicUrl constants

personioPublicUrlStr :: String
personioPublicUrlStr = personioPublicUrl ^. unpacked
