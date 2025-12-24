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

mkYesodDataOpts (setFocusOnNestedRoute (Just "NestR") defaultOpts) "App" nestedDispatchResources

instance YesodDispatchNested NestR where
    yesodDispatchNested _ () toParentRoute yre req =
        case drop 2 (W.pathInfo req) of
            [] -> Just $ \send ->
                yesodRunner getNestIndexR yre (Just (toParentRoute NestIndexR)) req send
            _ ->
                Nothing



getNestIndexR :: HandlerFor App Text
getNestIndexR = pure "getNestIndexR"

postNestIndexR :: HandlerFor App Text
postNestIndexR = pure "hello"
