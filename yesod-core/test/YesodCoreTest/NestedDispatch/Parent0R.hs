{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module YesodCoreTest.NestedDispatch.Parent0R where

import Data.Text (Text)
import qualified Data.Text as Text
import Yesod.Core.Handler
import YesodCoreTest.NestedDispatch.Resources
import Yesod.Core
import YesodCoreTest.NestedDispatch.Parent0R.Child0R

mkYesodOpts (setFocusOnNestedRoute (Just "Parent0R") defaultOpts) "App" nestedDispatchResources

getParent0IndexR :: Int -> HandlerFor App Text
getParent0IndexR = pure . Text.pack . show
