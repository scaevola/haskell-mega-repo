{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.GitHubSync.API where

import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Servant

type GitHubSyncAPI =
    SSOUser :> Get '[HTML] (HtmlPage "index")

githubSyncApi :: Proxy GitHubSyncAPI
githubSyncApi = Proxy
