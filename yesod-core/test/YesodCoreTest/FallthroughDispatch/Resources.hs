{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | Shared route table + foundation type for "YesodCoreTest.FallthroughDispatch.Runtime".
-- Two parents share the @\/foo@ prefix (@FirstFooR@\/@SecondFooR@), plus
-- @\/foo\/#Int@ and @\/foo\/#String@ parents — the deliberate overlap is why it
-- uses @parseRoutesNoCheck@ (the compile-time overlap check would otherwise
-- reject the shared prefix that the fallthrough behaviour exists to handle).
module YesodCoreTest.FallthroughDispatch.Resources where

import Yesod.Core
import Yesod.Routes.TH.Types

data App = App

fallthroughDispatchResources :: [ResourceTree String]
fallthroughDispatchResources = [parseRoutesNoCheck|

/foo   FirstFooR:
    /   FooIndexR
    /neat   FooNeatR

/foo   SecondFooR:
    /       NeverFiresR
    /other  OtherR

/foo/#Int  ParamFooR:
    /   FooWithIntR

/foo/#String TextFooR:
    /   FooWithTextR
|]
