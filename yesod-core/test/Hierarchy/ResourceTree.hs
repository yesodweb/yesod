{-# language TemplateHaskell #-}
{-# language QuasiQuotes #-}

module Hierarchy.ResourceTree where

import Yesod.Routes.Parse
import Yesod.Routes.TH

data Hierarchy = Hierarchy

hierarchyResources :: [ResourceTree String]
hierarchyResources = [parseRoutes|
/ HomeR GET

----------------------------------------

/!#Int BackwardsR GET

/admin/#Int AdminR:
    /            AdminRootR GET
    /login       LoginR     GET POST
    /table/#Text TableR     GET

/nest/ NestR !NestingAttr:

  /spaces      SpacedR   GET !NonNested

  /nest2 Nest2:
    /           GetPostR  GET POST
    /get        Get2      GET
    /post       Post2         POST
-- lol

  /nest3 Nest3:
    /get        Get3      GET
    /post       Post3         POST
--    /#Int       Delete3            DELETE

/afterwards AfterR !parent !key=value1:
  /             After     GET !child !key=value2

-- /trailing-nest TrailingNestR:
--  /foo TrailingFooR GET
--  /#Int TrailingIntR GET
|]
