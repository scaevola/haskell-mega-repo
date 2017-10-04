{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.FUM.Types.Email where

import Futurice.Prelude
import Kleene
import Prelude ()

emailKleene :: Kleene Char String
emailKleene = kleeneEverything <* ("@futurice.com" :: Kleene Char String)
