{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
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

module Route.RenderRouteSpec where

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
import Yesod.Routes.Class

data App = App

mkYesodData "App" [parseRoutes|

/foo/#Int   FooR:
    /   FooIndexR
    /bar/#String    FooBarR:
        /   FooBarIndexR
        /baz    FooBarBazR:
            /new    FooBarBazNewR
            /delete/#String FooBarBazDeleteR
            /get/#String    FooBarBazGetR

    |]

spec :: Spec
spec = do
    describe "renderRouteNested" $ do
        it "renders the entire route, not just end fragment" $ do
            renderRouteNested (3, "hello") (FooBarBazGetR "asdf")
                `shouldBe`
                    (["foo", "3", "bar", "hello", "baz", "get", "asdf"], [])
