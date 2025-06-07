{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module YesodCoreTest.MediaData where

import Yesod.Core

data Y = Y
mkYesodData "Y" [parseRoutes|
/ RootR GET
/static StaticR !IGNORED GET !alsoIgnored
|]
