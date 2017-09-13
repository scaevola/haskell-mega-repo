{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.FUM.Lomake where

import Control.Lens     (filtered)
import Futurice.Prelude
import Prelude ()

import Futurice.Lomake
import Futurice.App.FUM.Types

vEmployees :: (Login -> Bool) -> World -> V Login
vEmployees p world =
    vDynamic $ world ^.. worldEmployees . folded . filtered (p . view employeeLogin) . getter mk
  where
    mk e =
        ( e ^. employeeLogin
        , e ^. employeeName <> " (" <> e ^.employeeLogin . getter loginToText <> ")"
        )

vGroups :: (GroupName -> Bool) -> World -> V GroupName
vGroups p world =
    vDynamic $ world ^.. worldGroups . folded . filtered (p . view groupName) . getter mk
  where
    mk g =
        ( g ^. groupName
        , g ^. groupName . getter groupNameToText
        )
