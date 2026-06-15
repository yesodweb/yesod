{-# LANGUAGE OverloadedStrings #-}

-- | Regression coverage for the documented edge case: a nested parent route
-- with /no/ leading path piece (pattern @\/@) compiles to an unconditional
-- first match, so any sibling route declared after it is unreachable.
--
-- This is longstanding behaviour (the leaf-level overlap check never flagged
-- it, since the parent contributes zero pieces and the flattened leaf paths
-- @\/child@ vs @\/sibling@ do not overlap). The shadow is silent, so it is
-- pinned here.
module YesodCoreTest.ZeroPieceShadowRuntime
    ( specs
    ) where

import Test.Hspec
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import YesodCoreTest.ZeroPieceShadow.ShadowApp (ShadowApp (..))
import YesodCoreTest.RuntimeHarness (assertGet)

shadowReq :: HasCallStack => Int -> [Text] -> Maybe ByteString -> IO ()
shadowReq = assertGet ShadowApp

specs :: Spec
specs = describe "zero-piece nested parent shadowing siblings" $ do
    describe "the zero-piece parent shadows later siblings" $ do
        it "still dispatches the parent's own child (/child)" $
            shadowReq 200 ["child"] (Just "ShadowChildR")
        it "404s the shadowed sibling (/sibling), which the parent swallows" $
            shadowReq 404 ["sibling"] Nothing
