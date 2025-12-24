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

mkYesodDataOpts (setFocusOnNestedRoute (Just "Parent0R") defaultOpts) "App" nestedDispatchResources

instance YesodDispatchNested Parent0R where
    yesodDispatchNested _ i toParentRoute yre req =
        case drop parentPieceCount (W.pathInfo req) of
            [] ->
                Just $
                    let route =
                            toParentRoute ParentIndex0R
                        handler =
                            case W.requestMethod req of
                                "GET" ->
                                    fmap toTypedContent (getParent0IndexR i)
                                _ ->
                                    fmap (toTypedContent :: () -> TypedContent) badMethod
                    in
                        yesodRunner handler yre (Just route) req

            -- This branch is what happens if we have
            -- roNestedRouteFallthrough enabled. We create the view pattern
            -- for the route fragments, and then have a pattern guard `Just
            -- k <- yesodDispatchNested ...` This is what allows us to
            -- ensure that the match is successful.
            ["child0", fromPathPiece -> Just txt]
                | Just k <-
                    yesodDispatchNested
                        (Proxy :: Proxy Child0R)
                        (i, txt)
                        -- The `toParentRoute` is created by composing the
                        -- constructor of the parent with the
                        -- `toParentRoute` method we accept as an argument
                        (toParentRoute . Child0R txt)
                        yre
                        req
                -> Just k

            -- If we have roNestedRouteFallthrough = True, then generate
            -- a Nothing clause here
            _ ->
                Nothing

            -- If roNestedRouteFallthrough is False, then instead of doing
            -- the pattern guard, we just call to yesodDispatchNested directly. GHC
            -- will complain that this pattern is redundant. It is only here for Claude
            -- to read and generate the right code.
            ["child0", fromPathPiece -> Just txt] ->
                yesodDispatchNested
                    (Proxy :: Proxy Child0R)
                    (i, txt)
                    -- The `toParentRoute` is created by composing the
                    -- constructor of the parent with the
                    -- `toParentRoute` method we accept as an argument
                    (toParentRoute . Child0R txt)
                    yre
                    req
      where
        -- This is statically known from the route structure. Calculate
        -- this on the number of path pieces accounted for by the parent
        -- route.
        parentPieceCount =
            2

getParent0IndexR :: Int -> HandlerFor App Text
getParent0IndexR = pure . Text.pack . show
