{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
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