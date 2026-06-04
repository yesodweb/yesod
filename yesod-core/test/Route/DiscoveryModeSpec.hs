-- | Unit tests for the 'discoveryMode' classifier. It is a pure, total
-- function but previously had no direct coverage; every behavioural test only
-- exercised it indirectly through generated code.
module Route.DiscoveryModeSpec (spec) where

import Test.Hspec
import Yesod.Routes.TH.RenderRoute
    ( defaultOpts
    , setParameterizedSubroute
    , setFocusOnNestedRoute
    , DiscoveryMode (..)
    , discoveryMode
    )

spec :: Spec
spec = describe "discoveryMode" $ do
    -- Dimension 1: hasTyArgs == False (a monomorphic site is always nested).
    it "is NestedDiscovery for a monomorphic site, even with all opts off" $
        discoveryMode defaultOpts False `shouldBe` NestedDiscovery

    -- Dimensions 2/3 off, hasTyArgs on: the backwards-compatible inline case.
    it "is InlineCompat for a parameterized site with default opts" $
        discoveryMode defaultOpts True `shouldBe` InlineCompat

    -- Dimension 2: roParameterizedSubroute in the parameterized case.
    it "is NestedDiscovery for a parameterized site when parameterized-subroute is set" $
        discoveryMode (setParameterizedSubroute True defaultOpts) True
            `shouldBe` NestedDiscovery

    -- Dimension 3: roFocusOnNestedRoute in the parameterized case.
    it "is NestedDiscovery for a parameterized site when focusing on a nested route" $
        discoveryMode (setFocusOnNestedRoute (Just "Target") defaultOpts) True
            `shouldBe` NestedDiscovery

    it "stays NestedDiscovery for a monomorphic site with the flags set" $ do
        discoveryMode (setParameterizedSubroute True defaultOpts) False
            `shouldBe` NestedDiscovery
        discoveryMode (setFocusOnNestedRoute (Just "Target") defaultOpts) False
            `shouldBe` NestedDiscovery
