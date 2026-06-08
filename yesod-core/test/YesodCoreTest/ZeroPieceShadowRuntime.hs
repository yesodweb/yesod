{-# LANGUAGE OverloadedStrings #-}

-- | Regression coverage for the documented edge case: a nested parent route
-- with /no/ leading path piece (pattern @\/@) compiles to an unconditional
-- first match ('mkPathPat' @EndWild []@ = @WildP@), so any sibling route
-- declared after it is unreachable under the default
-- @'setNestedRouteFallthrough' False@. Enabling fallthrough lets a non-matching
-- subtree fall through to the siblings.
--
-- This is unchanged behaviour from previous releases (the leaf-level overlap
-- check never flagged it, since the parent contributes zero pieces and the
-- flattened leaf paths @\/child@ vs @\/sibling@ do not overlap). The
-- @EndWild@/@PathTail@ abstraction made the shadow silent, so it is pinned here.
module YesodCoreTest.ZeroPieceShadowRuntime
    ( specs
    ) where

import Test.Hspec
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import YesodCoreTest.ZeroPieceShadow.ShadowApp (ShadowApp (..))
import YesodCoreTest.ZeroPieceShadow.FallApp (FallApp (..))
import YesodCoreTest.RuntimeHarness (assertGet)

shadowReq :: HasCallStack => Int -> [Text] -> Maybe ByteString -> IO ()
shadowReq = assertGet ShadowApp

fallReq :: HasCallStack => Int -> [Text] -> Maybe ByteString -> IO ()
fallReq = assertGet FallApp

specs :: Spec
specs = describe "zero-piece nested parent shadowing siblings" $ do
    describe "default fallthrough = False (the parent's WildP shadows the sibling)" $ do
        it "still dispatches the parent's own child (/child)" $
            shadowReq 200 ["child"] (Just "ShadowChildR")
        it "404s the shadowed sibling (/sibling), which the WildP parent swallows" $
            shadowReq 404 ["sibling"] Nothing

    describe "setNestedRouteFallthrough True (the sibling becomes reachable)" $ do
        it "still dispatches the parent's own child (/child)" $
            fallReq 200 ["child"] (Just "FallChildR")
        it "falls through the WildP parent to the sibling (/sibling)" $
            fallReq 200 ["sibling"] (Just "FallSiblingR")
