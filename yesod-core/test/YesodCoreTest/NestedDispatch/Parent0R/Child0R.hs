{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -ddump-splices -ddump-to-file #-}

--
module YesodCoreTest.NestedDispatch.Parent0R.Child0R where

import Data.Text (Text)
import qualified Data.Text as Text
import Yesod.Core.Handler
import YesodCoreTest.NestedDispatch.Resources
import Yesod.Core

mkYesodOpts (setFocusOnNestedRoute (Just "Child0R") defaultOpts) "App" nestedDispatchResources

postParentChildIndexR :: Int -> Text -> HandlerFor App Text
postParentChildIndexR i t = pure $ Text.pack $ show (i, t)

getParentChildIndexR :: Int -> Text -> HandlerFor App Text
getParentChildIndexR = postParentChildIndexR

postParentChildR :: Int -> Text -> String -> HandlerFor App Text
postParentChildR i t c = pure $ Text.pack $ show (i, t, c)

getParentChildR :: Int -> Text -> String -> HandlerFor App Text
getParentChildR = postParentChildR
