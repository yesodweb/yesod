-- | Unit tests for 'checkNestedSubArity', the guard that gives
-- 'mkYesodSubDispatchInstance' an actionable error (instead of a cryptic kind
-- error) when a parameterized subsite is paired with unparameterized nested
-- route datatypes.
module Route.SubDispatchAritySpec (spec) where

import Test.Hspec
import Data.Either (isLeft)
import Yesod.Routes.TH.Internal (checkNestedSubArity)

spec :: Spec
spec = describe "checkNestedSubArity" $ do
    it "accepts a monomorphic subsite with an unparameterized nested datatype" $
        checkNestedSubArity "MySub" "NestedR" 0 0 `shouldBe` Right ()

    it "accepts a parameterized subsite whose nested datatype carries the param" $
        checkNestedSubArity "MySub" "NestedR" 1 1 `shouldBe` Right ()

    it "rejects when the nested datatype carries more params than the subsite" $
        -- Over-arity leaves the instance head partially applied (kind
        -- @Type -> ...@), so this must be rejected, not silently accepted.
        checkNestedSubArity "MySub" "NestedR" 1 2 `shouldSatisfy` isLeft

    it "rejects a parameterized subsite with an unparameterized nested datatype" $
        checkNestedSubArity "MySub" "NestedR" 1 0 `shouldSatisfy` isLeft

    it "names both types in the error message" $
        case checkNestedSubArity "MySub" "NestedR" 1 0 of
            Right () -> expectationFailure "expected a Left"
            Left msg -> do
                msg `shouldContain` "MySub"
                msg `shouldContain` "NestedR"
