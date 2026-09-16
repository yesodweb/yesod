{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans -Werror=incomplete-patterns #-}
module YesodCoreTest.RouteLeaf.Parameterized where

import Control.Monad (forM_)
import Test.Hspec
import Yesod.Core
import Yesod.Core.Class.Dispatch.ToParentRoute (toParentRoute)
import Yesod.Core.RouteLeaf
import YesodCoreTest.RouteLeaf.SplitResources
import YesodCoreTest.RouteLeaf.SplitData
import YesodCoreTest.RouteLeaf.CaptureData

-- Implicit type argument inference must agree with the explicit focused splice.
mkYesodDataOpts splitLeafOpts "SplitLeafApp" splitLeafResources


specs :: Spec
specs = describe "parameterized leaf views" $ do
    it "round-trips every owner through imported views with a hidden leaf constructor" $
        forM_
            [ TopR :: Route (SplitLeafApp ()), ScopeR 23 (LocalR "local")
            , ScopeR 42 (GroupR (ChildR "alice" (SplitItemR 7)))
            ] $ \route ->
                withRouteLeaf @ToParentRoute route (\args leaf -> toParentRoute args $ fromRouteLeaves leaf)
                    `shouldBe` route

    it "reuses views from a focused data splice" $ do
        let route = ScopeR 42 (GroupR (ChildR "alice" (SplitItemR 7))) :: Route (SplitLeafApp ())
        withRouteLeaf @Show route (\args leaf -> renderRouteNested args $ fromRouteLeaves leaf)
            `shouldBe` (["scope", "42", "group", "child", "alice", "item", "7"], [])

    it "packages the endpoint's owning fragment and its parent captures" $ do
        let route = ScopeR 42 (GroupR (ChildR "alice" (SplitItemR 7))) :: Route (SplitLeafApp ())
        case selectRouteLeaf route of
            SomeRouteLeaf FragmentChildR args leaves ->
                (args, fromRouteLeaves leaves) `shouldBe` ((42, "alice"), SplitItemR 7)
            _ -> expectationFailure "expected the endpoint's ChildR fragment"

    it "retains the focused mixed fragment's shallow projection" $ do
        let onLocal (LeafLocalR text) = text
        fillInNested onLocal "nested" (LocalR "hello" :: ScopeR ()) `shouldBe` "hello"
        fillInNested onLocal "nested" (GroupR (ChildR "alice" (SplitItemR 7)) :: ScopeR ()) `shouldBe` "nested"

    it "covers root endpoints alongside imported fragment views" $ do
        withRouteLeaf @Show (TopR :: Route (SplitLeafApp ())) (\_ leaf -> show $ fromRouteLeaves leaf)
            `shouldBe` "TopR"

    it "preserves type-variable captures and instance contexts" $ do
        let route = CapturedR 42 (ValueR 7) :: Route (CaptureApp Int)
        withRouteLeaf @ToParentRoute route (\args leaf -> toParentRoute args $ fromRouteLeaves leaf)
            `shouldBe` route
        withRouteLeaf @Show route (\args leaf -> renderRouteNested args $ fromRouteLeaves leaf)
            `shouldBe` (["captured", "42", "value", "7"], [])
