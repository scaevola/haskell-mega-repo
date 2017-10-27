{-# LANGUAGE TypeOperators #-}
module Futurice.App.ProxyMgmt.API where

import Futurice.Prelude
import Prelude ()

import Dashdo.Servant
import Futurice.Servant
import Servant

type ProxyMgmtAPI = SSOUser :> DashdoAPI

proxyMgmtApi :: Proxy ProxyMgmtAPI
proxyMgmtApi = Proxy
