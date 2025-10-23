{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module YesodCoreTest.NestedRouteFallThrough where

import Test.Hspec

import Data.Text
import Yesod.Core
import Network.Wai
import Network.Wai.Test


data App = App

mkYesodOpts (setNestedRouteFallThrough True defaultOpts) "App" [parseRoutes|

/home   FirstHomeR:
    /       FirstHomeIndexR POST
    /foo    FirstHomeFooR GET

/home/blah HomeBlahR POST

/home   SecondHomeR:
    /nope   SecondHomeNopeR POST

    |]

instance Yesod App

postHomeBlahR :: HandlerFor site Text
postHomeBlahR = pure "HomeBlahR"

getFirstHomeR :: HandlerFor site Text
getFirstHomeR = pure "FirstHomeR"

postFirstHomeIndexR :: HandlerFor site Text
postFirstHomeIndexR = pure "FirstHomeIndexR"

getFirstHomeFooR :: HandlerFor site Text
getFirstHomeFooR = pure "FirstHomeFooR"

postSecondHomeNopeR :: HandlerFor site Text
postSecondHomeNopeR = pure "SecondHomeNopeR"

spec :: Spec
spec = focus $ describe "NestedRouteFallThrough" $ do
    describe "/home/foo" $ do
        it "works" $ do
            runner $ do
                res <- request defaultRequest { pathInfo = ["home", "foo"] }
                assertStatus 200 res
                assertBody "FirstHomeFooR" res
    describe "/home/nope" $ do
        it "works" $ do
            runner $ do
                res <- request defaultRequest { pathInfo = ["home", "nope"] }
                assertStatus 200 res
                assertBody "SecondHomeNopeR" res

runner :: Session () -> IO ()
runner f = toWaiApp App >>= runSession f
