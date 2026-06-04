{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Runtime coverage for nested-route /fallthrough/ on a *parameterized* site.
-- The monomorphic fallthrough test ("YesodCoreTest.FallthroughDispatch")
-- exercises the @setNestedRouteFallthrough@ fallback only for an
-- unparameterized site; this is the parameterized counterpart, opted into
-- nested discovery so the parameterized subroute datatypes carry the type
-- variable while several same-prefix parents fall through to one another.
module YesodCoreTest.ParamFallthroughRuntime
    ( specs
    ) where

import Test.Hspec
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Yesod.Core

import YesodCoreTest.RuntimeHarness (assertRequest)

data PApp a = PApp a

mkYesodOpts
    (setNestedRouteFallthrough True (setParameterizedSubroute True defaultOpts))
    "PApp a"
    [parseRoutesNoCheck|
/foo   FirstFooR:
    /      FooIndexR
    /neat  FooNeatR

/foo   SecondFooR:
    /       NeverFiresR
    /other  OtherR

/foo/#Int  ParamFooR:
    /  FooWithIntR

/foo/#String TextFooR:
    /  FooWithTextR
|]

instance Yesod (PApp a) where
    messageLoggerSource = mempty

handleFooIndexR :: HandlerFor app String
handleFooIndexR = pure "FooIndexR"

handleOtherR :: HandlerFor app String
handleOtherR = pure "OtherR"

handleFooNeatR :: HandlerFor app String
handleFooNeatR = pure "FooNeatR"

handleNeverFiresR :: HandlerFor app String
handleNeverFiresR = pure "NeverFiresR"

handleFooWithIntR :: Int -> HandlerFor app String
handleFooWithIntR i = pure $ "FooWithIntR " <> show i

handleFooWithTextR :: String -> HandlerFor app String
handleFooWithTextR str = pure $ "FooWithTextR " <> str

testRequestIO :: HasCallStack => Int -> [Text] -> Maybe ByteString -> IO ()
testRequestIO status path mexpected =
    assertRequest (toWaiApp (PApp ())) "GET" status path mexpected

specs :: Spec
specs = describe "parameterized site, nested-route fallthrough" $ do
    it "matches the first parent's index (FooIndexR at /foo)" $
        testRequestIO 200 ["foo"] (Just "FooIndexR")
    it "matches a child of the first parent (FooNeatR at /foo/neat)" $
        testRequestIO 200 ["foo", "neat"] (Just "FooNeatR")
    it "falls through to a later same-prefix parent (OtherR at /foo/other)" $
        testRequestIO 200 ["foo", "other"] (Just "OtherR")
    it "falls through to the dynamic-prefix parent (FooWithIntR at /foo/3)" $
        testRequestIO 200 ["foo", "3"] (Just "FooWithIntR 3")
    it "falls through to the textual-prefix parent (FooWithTextR at /foo/asdf)" $
        testRequestIO 200 ["foo", "asdf"] (Just "FooWithTextR asdf")
