{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.FUM.Pages.Index (indexPage) where

import Prelude ()
import Futurice.Prelude
import Futurice.IdMap (IdMap)

import Futurice.App.FUM.Markup
import Futurice.App.FUM.Types

import qualified Personio

indexPage
    :: AuthUser
    -> World                    -- ^ the world
    -> IdMap Personio.Employee  -- ^ employees
    -> HtmlPage "indexpage"
indexPage auth _world _es = fumPage_ "FUM" auth $ do
    fumHeader_ "FUM" []

    fullRow_ $ 
        i_ "TBD"

    subheader_ "Management"

    -- TODO: it
    fullRow_ $
        futuLinkButton_ fromPersonioPageHref_ "Create employee"

