{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module YesodCoreTest.NestedDispatch.NestR where

import Data.Text (Text)
import Control.Monad (void)
import Yesod.Core.Handler
import Yesod.Core.Content
import YesodCoreTest.NestedDispatch.Resources
import Yesod.Routes.Parse
import Language.Haskell.TH
import Yesod.Routes.TH
import Yesod.Core.Class.Dispatch

mkRenderRouteInstanceOpts (setFocusOnNestedRoute (Just "NestR") defaultOpts) [] (ConT ''App) (map (fmap parseType) nestedDispatchResources)
mkRouteAttrsInstanceFor [] (ConT ''NestR) "NestR" $ map (fmap parseType) nestedDispatchResources
mkParseRouteInstanceFor "NestR" $ map (fmap parseType) nestedDispatchResources

getNestIndexR :: HandlerFor App Text
getNestIndexR = pure "getNestIndexR"

postNestIndexR :: HandlerFor App Text
postNestIndexR = pure "hello"

instance YesodDispatchNested NestR where
    type ParentArgs NestR = ()
    type ParentSite NestR = App
    yesodDispatchNested () method routes =
        helper routes
      where
        helper [] =
            case method of
                "GET" ->
                    ( fmap toTypedContent $ getNestIndexR
                    , Just NestIndexR
                    )
                "POST" ->
                    ( fmap toTypedContent $ postNestIndexR
                    , Just NestIndexR
                    )
                _ ->
                    ( fmap toTypedContent $ void $ badMethod
                    , Just NestIndexR
                    )
        helper _ =
            ( fmap toTypedContent $ void notFound
            , Nothing
            )

