{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module YesodCoreTest.NestedDispatch.ParentR where

import Data.Tuple (Solo(..))
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

mkRenderRouteInstanceOpts (setFocusOnNestedRoute (Just "ParentR") defaultOpts) [] (ConT ''App) (map (fmap parseType) nestedDispatchResources)
mkRouteAttrsInstanceFor [] (ConT ''ParentR) "ParentR" $ map (fmap parseType) nestedDispatchResources
mkParseRouteInstanceFor "ParentR" $ map (fmap parseType) nestedDispatchResources

handleChild1R :: Int -> Text -> HandlerFor App  Text
handleChild1R i t = return (Text.pack (show i) <> t)

getChild2R :: Int -> Int -> HandlerFor App Text
getChild2R i0 i1 = return ("GET" <> Text.pack (show (i0, i1)))

postChild2R :: Int -> Int -> HandlerFor App Text
postChild2R i0 i1 = return ("POST" <> Text.pack (show (i0, i1)))

instance YesodDispatchNested ParentR where
    type ParentArgs ParentR = Solo Int
    type ParentSite ParentR = App
    yesodDispatchNested (MkSolo parentDyn) method routes =
        helper routes
      where
        helper ((fromPathPiece -> Just dyn0) : "child1" : []) =
            let mk h = (fmap toTypedContent h, Just (Child1R dyn0))
            in mk $ handleChild1R parentDyn dyn0
        helper ((fromPathPiece -> Just dyn0) : "child2" : []) =
            let mk h = (h, Just (Child2R dyn0))
            in case method of
                    "GET" ->
                        mk $ fmap toTypedContent $ getChild2R parentDyn dyn0
                    "POST" ->
                        mk $ fmap toTypedContent $ postChild2R parentDyn dyn0
                    _ ->
                        mk $ fmap toTypedContent $ void $ badMethod
        helper _ =
            ( fmap toTypedContent $ void notFound
            , Nothing
            )

