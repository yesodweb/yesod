{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Routes where

import           Yesod.Core

data App = App

mkYesodData "App" [parseRoutes|
/ HomeR GET
/some/thing SomethingR GET !CACHE
/appcache AppCacheR GET
|]
