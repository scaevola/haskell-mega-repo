{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.FUM.Pages.Index (indexPage) where

import Futurice.IdMap   (IdMap)
import Futurice.Prelude
import Prelude ()

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

    when (hasITRights auth) $ do
        subheader_ "IT Tasks"

        fullRow_ $
            futuLinkButton_ fromPersonioPageHref_ "Create employee"
