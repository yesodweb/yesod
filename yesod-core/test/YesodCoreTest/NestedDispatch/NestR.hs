{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -ddump-splices -ddump-to-file #-}

module YesodCoreTest.NestedDispatch.NestR where

import Data.Text (Text)
import Yesod.Core.Handler
import YesodCoreTest.NestedDispatch.Resources
import Language.Haskell.TH (Type(ConT))
import Yesod.Routes.Parse
import Yesod.Routes.TH (mkRenderRouteInstanceOpts, mkRouteAttrsInstanceFor, mkParseRouteInstanceFor)
import Yesod.Core.Internal.TH

mkYesodOpts (setFocusOnNestedRoute (Just "NestR") defaultOpts) "App" nestedDispatchResources

getNestIndexR :: HandlerFor App Text
getNestIndexR = pure "getNestIndexR"

postNestIndexR :: HandlerFor App Text
postNestIndexR = pure "hello"

-- instance YesodDispatchNested NestR where
--     type ParentArgs NestR = ()
--     type ParentSite NestR = App
--     yesodDispatchNested () method routes =
--         helper routes
--       where
--         helper [] =
--             case method of
--                 "GET" ->
--                     ( fmap toTypedContent $ getNestIndexR
--                     , Just NestIndexR
--                     )
--                 "POST" ->
--                     ( fmap toTypedContent $ postNestIndexR
--                     , Just NestIndexR
--                     )
--                 _ ->
--                     ( fmap toTypedContent $ void $ badMethod
--                     , Just NestIndexR
--                     )
--         helper _ =
--             ( fmap toTypedContent $ void notFound
--             , Nothing
--             )
--
