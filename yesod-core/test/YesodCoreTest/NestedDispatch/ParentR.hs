{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -ddump-to-file -ddump-splices #-}
--

module YesodCoreTest.NestedDispatch.ParentR where

import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad (void)
import Yesod.Core.Handler
import Yesod.Core.Content
import YesodCoreTest.NestedDispatch.Resources
import Yesod.Routes.Parse
import Language.Haskell.TH
import Yesod.Routes.TH
import Yesod.Core.Class.Dispatch
import Web.PathPieces
import Yesod.Core.Internal.TH

mkRenderRouteInstanceOpts (setFocusOnNestedRoute (Just "ParentR") defaultOpts) [] (ConT ''App) (map (fmap parseType) nestedDispatchResources)
mkRouteAttrsInstanceFor [] (ConT ''ParentR) "ParentR" $ map (fmap parseType) nestedDispatchResources
mkParseRouteInstanceFor "ParentR" $ map (fmap parseType) nestedDispatchResources

mkYesodDispatchOpts (setFocusOnNestedRoute (Just "ParentR") defaultOpts) "App" nestedDispatchResources

handleChild1R :: Int -> Text -> HandlerFor App  Text
handleChild1R i t = return (Text.pack (show i) <> t)

getChild2R :: Int -> Int -> HandlerFor App Text
getChild2R i0 i1 = return ("GET" <> Text.pack (show (i0, i1)))

postChild2R :: Int -> Int -> HandlerFor App Text
postChild2R i0 i1 = return ("POST" <> Text.pack (show (i0, i1)))
