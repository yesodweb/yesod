{-# language TemplateHaskell #-}
{-# language QuasiQuotes #-}

-- | Shared resource tree + foundation type for the @Hierarchy@ split-codegen
-- fixture. The @Hierarchy.*@ sibling modules each focus on one nested parent
-- (NestR, Nest2, Nest3, NestInner, AdminR) via @setFocusOnNestedRoute@ and emit
-- that fragment's instances separately, while "Hierarchy" itself emits the
-- whole-tree instances; keeping the tree here lets every splice share it
-- without tripping the TH stage restriction. Used by the @test-routes@ suite.
module Hierarchy.ResourceTree where

import Yesod.Routes.Parse
import Language.Haskell.TH (Type)
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
    /nest-inner NestInner:
        /       NestInnerIndexR GET
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

hierarchyResourcesWithType :: [ResourceTree Type]
hierarchyResourcesWithType = map (fmap parseType) hierarchyResources
