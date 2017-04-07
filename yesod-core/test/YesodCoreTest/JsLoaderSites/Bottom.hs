{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module YesodCoreTest.JsLoaderSites.Bottom (B(..)) where
    ( B(..)
    , Widget
    , resourcesB -- avoid warning
    ) where

import Yesod.Core
import Yesod.Core.Widget

data B = B
mkYesod "B" [parseRoutes|
/ BottomR GET
|]
instance Yesod B where
  jsLoader _ = BottomOfBody

getBottomR :: Handler Html
getBottomR = defaultLayout $ addScriptRemote "load.js"
