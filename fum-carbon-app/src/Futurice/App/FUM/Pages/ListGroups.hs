{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.FUM.Pages.ListGroups (listGroupsPage) where

import Control.Lens     (Getting, forOf_, hasn't)
import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.Markup
import Futurice.App.FUM.Types

listGroupsPage
    :: AuthUser
    -> World     -- ^ the world
    -> HtmlPage "list-groups"
listGroupsPage auth world = fumPage_ "Groups" auth $ do
    -- Title
    fumHeader_ "Groups" []

    fullRow_ $
        futuLinkButton_ createGroupHref_ "Create group"

    when (hasn't (worldGroups . folded) world) $
        row_ $ large_ 12 [ class_ "callout warning" ] $
            em_ "No groups"

    fullRow_ $ table_ $ do
        thead_ $ tr_ $ do
            th_ "Name"
            th_ "Type"

        tbody_ $ forOf_ (sortedOnOf (view groupName) $ worldGroups . folded) world $ \g -> tr_ $ do
            td_ $ a_ [ viewGroupHref_ $ g ^. groupName] $ toHtml $ g ^. groupName
            td_ $ toHtml $ g ^. groupType

-- | This isn't super effective, yet good enough.
sortedOnOf
    :: (Ord b, Contravariant f, Applicative f)
    => (a -> b)
    -> Getting (Endo [a]) s a
    -> LensLike' f s a
sortedOnOf m l f s = phantom $ traverse_ f $ sortOn m $ s ^.. l
