{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- No policy instances are in scope: the generated instance retains the
-- constraint until an application or request chooses this fragment.
module YesodCoreTest.RouteLeafHook.Account where

import Data.Text (Text)
import Data.Proxy (Proxy(..))
import Yesod.Core
import YesodCoreTest.RouteLeafHook.Options
import YesodCoreTest.RouteLeafHook.Foundation

mkYesodDispatchOpts
    (setFocusOnNestedRoute "AccountR" hookRouteOpts)
    "LeafApp"
    resourcesLeafApp

-- This function also compiles without YesodDispatch LeafApp in scope.
accountApp :: AuthorizeRoute AccountR => LeafApp -> IO Application
accountApp = toWaiAppPlainNested (Proxy :: Proxy AccountR) (42, "alice")

getItemR :: Int -> Text -> Int -> HandlerFor LeafApp String
getItemR _ _ _ = recordEvent "handler" >> pure "item"

postItemR :: Int -> Text -> Int -> HandlerFor LeafApp String
postItemR _ _ _ = recordEvent "handler" >> pure "posted"

getFilesR :: Int -> Text -> [Text] -> HandlerFor LeafApp String
getFilesR _ _ _ = recordEvent "handler" >> pure "files"

getRequiredR :: Int -> Text -> HandlerFor LeafApp String
getRequiredR _ _ = recordEvent "handler" >> pure "required"

getErrorR :: Int -> Text -> HandlerFor LeafApp String
getErrorR _ _ = recordEvent "handler" >> invalidArgs ["bad input"]
