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

mkYesodOpts (setFocusOnNestedRoute (Just "NestR") defaultOpts) "App" nestedDispatchResources

getNestIndexR :: HandlerFor App Text
getNestIndexR = pure "getNestIndexR"

postNestIndexR :: HandlerFor App Text
postNestIndexR = pure "hello"
