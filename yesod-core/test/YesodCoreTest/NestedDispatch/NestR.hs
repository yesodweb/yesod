{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module YesodCoreTest.NestedDispatch.NestR where

import Yesod.Core
import Data.Text (Text)
import YesodCoreTest.NestedDispatch.Resources
import qualified Network.Wai as W
import Yesod.Core.Class.Dispatch

mkYesodOpts (setFocusOnNestedRoute (Just "NestR") defaultOpts) "App" nestedDispatchResources

getNestIndexR :: HandlerFor App Text
getNestIndexR = pure "getNestIndexR"

postNestIndexR :: HandlerFor App Text
postNestIndexR = pure "hello"
