{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module YesodCoreTest.JsLoaderSites.HeadAsync (HA(..)) where

import Yesod.Core

data HA = HA
mkYesod "HA" [parseRoutes|
/ HeadAsyncR GET
|]
instance Yesod HA where
  jsLoader _ = BottomOfHeadAsync $ loadJsYepnope $ Left "yepnope.js"

getHeadAsyncR :: Handler RepHtml
getHeadAsyncR = defaultLayout $ addScriptRemote "load.js"
