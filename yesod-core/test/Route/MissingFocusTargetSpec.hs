{-# LANGUAGE TemplateHaskell #-}

-- | Exercises the TH failure paths that fire when a 'setFocusOnNestedRoute'
-- target does not exist in the resource tree. Historically the 'ParseRoute'
-- and 'RouteAttrs' generators silently produced an always-'Nothing' \/
-- always-'mempty' nested instance for a bogus target, while the 'Dispatch' and
-- 'RenderRoute' generators correctly 'fail'ed. All four are now routed through
-- the shared 'findNestedRoute' lookup and 'fail' with the same message.
--
-- A 'Q' 'fail' raises its message through 'qReport' rather than carrying it in
-- the thrown value (so 'runQ' just reports @\"Q monad failure\"@), so — as in
-- "Route.DeepAritySpec" — we assert at compile time with 'recover': each
-- generator is run inside a splice against a nonexistent target and must take
-- its @fail@ path (driving 'recover' to its handler). The shared lookup that
-- produces the \"was not found in resources\" message is unit-tested directly
-- via 'findNestedRoute'.
module Route.MissingFocusTargetSpec (spec) where

import Test.Hspec
import Data.Maybe (isNothing)
import Language.Haskell.TH (recover)

import Yesod.Routes.TH.Types (resourceTreeName, TyArgs(NoTyArgs))
import Yesod.Routes.TH.ParseRoute (mkParseRouteInstanceFor)
import Yesod.Routes.TH.RouteAttrs (mkRouteAttrsInstanceFor)
import Yesod.Routes.TH.RenderRoute
    (mkRenderRouteInstanceOpts, defaultOpts, setFocusOnNestedRoute)
import Yesod.Routes.TH.Dispatch (mkDispatchInstance)
import Yesod.Routes.TH.Internal (findNestedRoute)
import Route.MissingFocusTargetTypes (sampleTree, bogusType, bogusName)

-- | 'True' iff the named generator 'fail'ed on the bogus focus target (i.e.
-- 'recover' fell through to its @True@ handler); 'False' if it wrongly produced
-- declarations. Each is forced at compile time so a regression to a silent
-- always-empty instance flips the corresponding boolean.

parseRouteFailed :: Bool
parseRouteFailed =
    $(recover [| True |] $ do
        _ <- mkParseRouteInstanceFor bogusName sampleTree
        [| False |])

routeAttrsFailed :: Bool
routeAttrsFailed =
    $(recover [| True |] $ do
        _ <- mkRouteAttrsInstanceFor [] bogusType bogusName sampleTree
        [| False |])

renderRouteFailed :: Bool
renderRouteFailed =
    $(recover [| True |] $ do
        _ <- mkRenderRouteInstanceOpts
                (setFocusOnNestedRoute (Just bogusName) defaultOpts)
                [] NoTyArgs bogusType sampleTree
        [| False |])

dispatchFailed :: Bool
dispatchFailed =
    $(recover [| True |] $ do
        _ <- mkDispatchInstance
                (setFocusOnNestedRoute (Just bogusName) defaultOpts)
                bogusType [] NoTyArgs pure sampleTree
        [| False |])

spec :: Spec
spec = describe "missing focus target" $ do
    describe "fails every nested generator (recover path)" $ do
        it "ParseRoute (mkParseRouteInstanceFor)" $
            parseRouteFailed `shouldBe` True
        it "RouteAttrs (mkRouteAttrsInstanceFor)" $
            routeAttrsFailed `shouldBe` True
        it "RenderRoute (mkRenderRouteInstanceOpts)" $
            renderRouteFailed `shouldBe` True
        it "Dispatch (mkDispatchInstance)" $
            dispatchFailed `shouldBe` True

    describe "shared lookup (findNestedRoute)" $ do
        it "returns Nothing for a target absent from the tree" $
            isNothing (findNestedRoute bogusName sampleTree)
                `shouldBe` True
        it "returns the subtree for a target present in the tree" $
            (fmap (map resourceTreeName . snd) (findNestedRoute "ParentR" sampleTree))
                `shouldBe` Just ["ChildR"]
