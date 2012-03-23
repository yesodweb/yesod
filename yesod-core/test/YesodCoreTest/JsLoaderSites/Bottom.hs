{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module YesodCoreTest.JsLoaderSites.Bottom (B(..), Widget) where

import Yesod.Core

data B = B
mkYesod "B" [parseRoutes|
/ BottomR GET
|]
instance Yesod B where
  jsLoader _ = BottomOfBody

getBottomR :: Handler RepHtml
getBottomR = defaultLayout $ addScriptRemote "load.js"

