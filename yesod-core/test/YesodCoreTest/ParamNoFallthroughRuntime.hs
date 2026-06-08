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

-- | The negative counterpart to "YesodCoreTest.ParamFallthroughRuntime".
--
-- That module exercises nested-route fallthrough with the flag /enabled/ —
-- every assertion expects fallthrough to succeed. This module builds the same
-- overlapping-@\/foo@ parents *without* 'setNestedRouteFallthrough', and
-- asserts a 404 exactly where the enabled version falls through to a later
-- same-prefix parent. Without this, a regression that made nested fallthrough
-- always-on (ignoring 'roNestedRouteFallthrough') would pass every test.
module YesodCoreTest.ParamNoFallthroughRuntime
    ( specs
    ) where

import Test.Hspec
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Yesod.Core

import YesodCoreTest.RuntimeHarness (assertGet)

data PNApp a = PNApp a

-- Same routes as ParamFallthroughRuntime's first two parents, but generated
-- WITHOUT setNestedRouteFallthrough: the first parent that matches the @/foo@
-- prefix is committed to, so a path its children can't match 404s rather than
-- falling through to the next @/foo@ parent.
mkYesodOpts
    (setParameterizedSubroute True defaultOpts)
    "PNApp a"
    [parseRoutesNoCheck|
/foo   FirstFooR:
    /      FooIndexR
    /neat  FooNeatR

/foo   SecondFooR:
    /       NeverFiresR
    /other  OtherR
|]

instance Yesod (PNApp a) where
    messageLoggerSource = mempty

handleFooIndexR :: HandlerFor app String
handleFooIndexR = pure "FooIndexR"

handleFooNeatR :: HandlerFor app String
handleFooNeatR = pure "FooNeatR"

handleNeverFiresR :: HandlerFor app String
handleNeverFiresR = pure "NeverFiresR"

handleOtherR :: HandlerFor app String
handleOtherR = pure "OtherR"

testRequestIO :: HasCallStack => Int -> [Text] -> Maybe ByteString -> IO ()
testRequestIO = assertGet (PNApp ())

specs :: Spec
specs = describe "parameterized site, nested-route fallthrough DISABLED" $ do
    it "still matches the first parent's index (FooIndexR at /foo)" $
        testRequestIO 200 ["foo"] (Just "FooIndexR")
    it "still matches a child of the first parent (FooNeatR at /foo/neat)" $
        testRequestIO 200 ["foo", "neat"] (Just "FooNeatR")
    it "404s instead of falling through to a later same-prefix parent (/foo/other)" $
        -- With fallthrough this is OtherR (200); without it, the first /foo
        -- parent commits and its children don't match "other".
        testRequestIO 404 ["foo", "other"] Nothing
