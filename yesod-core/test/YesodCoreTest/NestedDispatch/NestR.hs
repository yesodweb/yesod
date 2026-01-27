{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -ddump-splices #-}

module YesodCoreTest.NestedDispatch.NestR where

import Yesod.Core
import Data.Text (Text)
import YesodCoreTest.NestedDispatch.Resources

mkYesodOpts (setFocusOnNestedRoute (Just "NestR") defaultOpts) "App" nestedDispatchResources

getNestIndexR :: HandlerFor App Text
getNestIndexR = pure "getNestIndexR"

postNestIndexR :: HandlerFor App String
postNestIndexR = pure "hello"
