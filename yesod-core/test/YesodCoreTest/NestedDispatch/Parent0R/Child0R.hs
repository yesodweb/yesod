{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | Leaf handlers for the grandchild parent @Child0R@ of the
-- "YesodCoreTest.NestedDispatch" demo. Non-obvious: this module defines /only/
-- the handlers — the @Child0R@ datatype and its dispatch instance are emitted
-- by the focused @Parent0R@ splice (one level up), since focusing on a parent
-- also generates its descendants' nested instances.
module YesodCoreTest.NestedDispatch.Parent0R.Child0R where

import Data.Text (Text)
import qualified Data.Text as Text
import Yesod.Core.Handler
import YesodCoreTest.NestedDispatch.Resources
import Yesod.Core

postParentChildIndexR :: Int -> Text -> HandlerFor App Text
postParentChildIndexR i t = pure $ Text.pack $ show (i, t)

getParentChildIndexR :: Int -> Text -> HandlerFor App Text
getParentChildIndexR = postParentChildIndexR

postParentChildR :: Int -> Text -> String -> HandlerFor App Text
postParentChildR i t c = pure $ Text.pack $ show (i, t, c)

getParentChildR :: Int -> Text -> String -> HandlerFor App Text
getParentChildR = postParentChildR
