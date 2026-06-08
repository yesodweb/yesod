{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Exercises the *delegated* (nested-instance) no-fallthrough dispatch path
-- for a 'ResourceParent' child.
--
-- "YesodCoreTest.ParamNoFallthroughRuntime" is two levels deep (parent ->
-- leaf), so the parent's generated @YesodDispatchNested@ instance only ever
-- dispatches to leaf children. This module goes three levels deep
-- (grandparent -> parent -> leaf) under nested discovery (the site carries a
-- type parameter), so the grandparent's nested instance dispatches to the
-- *inner parent* via 'genNestedDispatchClauses' @ResourceParent@ arm — the arm
-- that, with fallthrough disabled, must convert an inner @Nothing@ into a 404
-- commit rather than returning the raw @Maybe@ (which under an outer
-- fallthrough caller would leak through to sibling routes).
module YesodCoreTest.ParamNestedNoFallthroughRuntime
    ( specs
    ) where

import Test.Hspec
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Yesod.Core

import YesodCoreTest.RuntimeHarness (assertGet)

data PNNApp a = PNNApp a

-- Three-level nesting, generated WITHOUT setNestedRouteFallthrough. A request
-- that matches the grandparent and inner-parent prefixes but no leaf must
-- commit to a 404 (the inner parent is delegated to via its own nested
-- dispatch instance), never fall through.
mkYesodOpts
    (setParameterizedSubroute True defaultOpts)
    "PNNApp a"
    [parseRoutesNoCheck|
/grand   GrandR:
    /inner  InnerR:
        /leaf   LeafR
        /also   AlsoR
|]

instance Yesod (PNNApp a) where
    messageLoggerSource = mempty

handleLeafR :: HandlerFor app String
handleLeafR = pure "LeafR"

handleAlsoR :: HandlerFor app String
handleAlsoR = pure "AlsoR"

testRequestIO :: HasCallStack => Int -> [Text] -> Maybe ByteString -> IO ()
testRequestIO = assertGet (PNNApp ())

specs :: Spec
specs = describe "parameterized site, deep nesting, fallthrough DISABLED" $ do
    it "matches a leaf under the inner parent (/grand/inner/leaf)" $
        testRequestIO 200 ["grand", "inner", "leaf"] (Just "LeafR")
    it "matches the inner parent's other leaf (/grand/inner/also)" $
        testRequestIO 200 ["grand", "inner", "also"] (Just "AlsoR")
    it "404s when the grandparent+parent prefix match but no leaf does (/grand/inner/nope)" $
        -- Drives the delegated ResourceParent no-fallthrough arm: GrandR's
        -- nested instance dispatches to InnerR's nested instance, which returns
        -- Nothing for "nope"; that must become a committed 404.
        testRequestIO 404 ["grand", "inner", "nope"] Nothing
    it "404s when only the grandparent prefix matches (/grand/nope)" $
        testRequestIO 404 ["grand", "nope"] Nothing
