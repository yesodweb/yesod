{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -ddump-splices -ddump-to-file #-}

module YesodCoreTest.NestedDispatch.Parent0R where

import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as Text
import Yesod.Core.Handler
import YesodCoreTest.NestedDispatch.Resources
import Yesod.Core
import YesodCoreTest.NestedDispatch.Parent0R.Child0R
import qualified Network.Wai as W
import Yesod.Core.Class.Dispatch

mkYesodOpts (setFocusOnNestedRoute (Just "Parent0R") defaultOpts) "App" nestedDispatchResources

getParent0IndexR :: Int -> HandlerFor App Text
getParent0IndexR = pure . Text.pack . show
