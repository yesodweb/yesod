{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module YesodCoreTest.NestedDispatch.ParentR where

import Data.Text (Text)
import qualified Data.Text as Text
import Yesod.Core.Handler
import YesodCoreTest.NestedDispatch.Resources
import Yesod.Core

mkYesodOpts (setFocusOnNestedRoute (Just "ParentR") defaultOpts) "App" nestedDispatchResources

handleChild1R :: Int -> Text -> HandlerFor App  Text
handleChild1R i t = return (Text.pack (show i) <> t)

getChild2R :: Int -> Int -> HandlerFor App Text
getChild2R i0 i1 = return ("GET" <> Text.pack (show (i0, i1)))

postChild2R :: Int -> Int -> HandlerFor App Text
postChild2R i0 i1 = return ("POST" <> Text.pack (show (i0, i1)))
