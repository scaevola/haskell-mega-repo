{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.FUM.MachineAPI (
    FUMMachineAPI,
    fumMachineApi,
    ) where

import FUM.Types.GroupName (GroupName)
import FUM.Types.Login     (Login)
import Futurice.Prelude
import Prelude ()
import Servant.API

-------------------------------------------------------------------------------
-- Servant API
-------------------------------------------------------------------------------

type FUMMachineAPI =
    "groups" :> Capture "group-name" GroupName :> "employees" :> Get '[JSON] (Set Login)

fumMachineApi :: Proxy FUMMachineAPI
fumMachineApi = Proxy

-------------------------------------------------------------------------------
-- Haxl
-------------------------------------------------------------------------------

-- todo
