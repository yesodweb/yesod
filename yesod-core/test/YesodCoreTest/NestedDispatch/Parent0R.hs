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

-- | A multi-piece (@*Texts@) leaf nested under a split-out parent. This is the
-- regression fixture for the nested-dispatch generator dropping the trailing
-- multipiece (it hardcoded 'EndExact' and built the constructor one arg short),
-- which fails to compile this focused dispatch unless the multi is bound,
-- appended to the route constructor, and forwarded to the handler.
getFilesR :: Int -> [Text] -> HandlerFor App Text
getFilesR i ts = return (Text.pack (show i) <> ":" <> Text.pack (show ts))
