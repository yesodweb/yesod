-- | Unit tests for 'checkNestedSubArity', the guard that gives
-- 'mkYesodSubDispatchInstance' an actionable error (instead of a cryptic kind
-- error) when a parameterized subsite is paired with unparameterized nested
-- route datatypes.
module Route.SubDispatchAritySpec (spec) where

import Test.Hspec
import Data.Maybe (isJust)
import Yesod.Routes.TH.Internal
    ( checkNestedSubArity
    , arityMismatchMessage
    , SubsiteName(..)
    , RouteName(..)
    , SubsiteArity(..)
    , RouteArity(..)
    )

-- | The newtype wrappers are positional noise in the tests; this names the
-- call the way the production caller does. 'Nothing' means the arities match;
-- 'Just' carries the rendered mismatch message.
check :: String -> String -> Int -> Int -> Maybe String
check sub route subArgs routeArity =
    fmap arityMismatchMessage $
        checkNestedSubArity
            (SubsiteName sub)
            (RouteName route)
            (SubsiteArity subArgs)
            (RouteArity routeArity)

spec :: Spec
spec = describe "checkNestedSubArity" $ do
    it "accepts a monomorphic subsite with an unparameterized nested datatype" $
        check "MySub" "NestedR" 0 0 `shouldBe` Nothing

    it "accepts a parameterized subsite whose nested datatype carries the param" $
        check "MySub" "NestedR" 1 1 `shouldBe` Nothing

    it "rejects when the nested datatype carries more params than the subsite" $
        -- Over-arity leaves the instance head partially applied (kind
        -- @Type -> ...@), so this must be rejected, not silently accepted.
        check "MySub" "NestedR" 1 2 `shouldSatisfy` isJust

    it "rejects a parameterized subsite with an unparameterized nested datatype" $
        check "MySub" "NestedR" 1 0 `shouldSatisfy` isJust

    it "names both types in the error message" $
        case check "MySub" "NestedR" 1 0 of
            Nothing -> expectationFailure "expected a mismatch"
            Just msg -> do
                msg `shouldContain` "MySub"
                msg `shouldContain` "NestedR"
