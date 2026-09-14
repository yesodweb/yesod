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
import Yesod.Routes.TH.Types (TyArgs, toTyArgs)
import Language.Haskell.TH (Type (ConT), mkName)

-- | A site with no type parameters ('hasTyArgs' 'False').
noArgs :: TyArgs
noArgs = toTyArgs []

-- | A site with one type parameter ('hasTyArgs' 'True').
someArgs :: TyArgs
someArgs = toTyArgs [(ConT (mkName "Int"), mkName "a")]

spec :: Spec
spec = describe "discoveryMode" $ do
    -- Dimension 1: no type args (a monomorphic site is always nested).
    it "is NestedDiscovery for a monomorphic site, even with all opts off" $
        discoveryMode defaultOpts noArgs `shouldBe` NestedDiscovery

    -- Dimensions 2/3 off, has type args: the backwards-compatible inline case.
    it "is InlineCompat for a parameterized site with default opts" $
        discoveryMode defaultOpts someArgs `shouldBe` InlineCompat

    -- Dimension 2: roParameterizedSubroute in the parameterized case.
    it "is NestedDiscovery for a parameterized site when parameterized-subroute is set" $
        discoveryMode (setParameterizedSubroute True defaultOpts) someArgs
            `shouldBe` NestedDiscovery

    -- Dimension 3: roFocusOnNestedRoute in the parameterized case.
    it "is NestedDiscovery for a parameterized site when focusing on a nested route" $
        discoveryMode (setFocusOnNestedRoute "Target" defaultOpts) someArgs
            `shouldBe` NestedDiscovery

    it "stays NestedDiscovery for a monomorphic site with the flags set" $ do
        discoveryMode (setParameterizedSubroute True defaultOpts) noArgs
            `shouldBe` NestedDiscovery
        discoveryMode (setFocusOnNestedRoute "Target" defaultOpts) noArgs
            `shouldBe` NestedDiscovery
