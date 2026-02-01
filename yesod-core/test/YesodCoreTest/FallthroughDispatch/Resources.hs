{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module YesodCoreTest.FallthroughDispatch.Resources where

import Yesod.Core
import Yesod.Routes.TH.Types

data App = App

-- TODO: make parseRoutesNoCheck smarter
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
