{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module YesodCoreTest.SubSubData where

import Yesod.Core


data OuterSubSite = OuterSubSite { getInner :: InnerSubSite }

data InnerSubSite = InnerSubSite

mkYesodSubData "InnerSubSite" [parseRoutes|
/ SubR GET
|]

mkYesodSubData "OuterSubSite" [parseRoutes|
/ InnerSubSiteR InnerSubSite getInner
|]