{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns#-}

module Route.FallthroughSpec where

import Network.Wai
import Network.Wai.Test
import Yesod.Core
import Test.Hspec
import Data.Foldable (for_)
import Test.Hspec
import Network.Wai
import Network.Wai.Test
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Char8 as S8
import Data.String (IsString)
import Data.Text (Text)
import Data.Maybe (fromJust)
import Data.Monoid (Endo (..))
import qualified Control.Monad.Trans.Writer    as Writer
import qualified Data.Set as Set
import Yesod.Routes.TH
import Language.Haskell.TH

data App = App

do
    let resources = [parseRoutesNoCheck|

/ HomeR
/foo FirstFooR:
    /   FooIndexR
/foo SecondFooR:
    /blah FooBlahR
    /baz  FooBaz1R:
        /   FooBazIndexR
    /baz    FooBaz2R:
        /foo    FooBaz2FooR

    |]

    let opts = setNestedRouteFallthrough True defaultOpts
        appType = ConT (mkName "App")
    render <- mkRenderRouteInstanceOpts opts [] [] appType resources
    parse <- mkParseRouteInstanceOpts opts [] appType resources
    pure (render <> parse)

spec :: Spec
spec = do
    describe "parseRoute" $ do
        let routeShouldParse path result =
                parseRoute (path, []) `shouldBe` Just result
        it "can fall through" $ do
            routeShouldParse ["foo", "blah"] (SecondFooR FooBlahR)
        it "nested fallthrough works too" $ do
            routeShouldParse ["foo", "baz", "foo"] (SecondFooR (FooBaz2R FooBaz2FooR))
