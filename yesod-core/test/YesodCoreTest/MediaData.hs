{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module YesodCoreTest.MediaData where

import Yesod.Core

data Y = Y
mkYesodData "Y" [parseRoutes|
/ RootR GET
/static StaticR GET
|]
