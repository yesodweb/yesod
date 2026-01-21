{-# LANGUAGE CPP #-}

module YesodCoreTest (specs) where

import YesodCoreTest.CleanPath
import YesodCoreTest.Exceptions
import YesodCoreTest.Widget
import YesodCoreTest.Media
import YesodCoreTest.Meta
import YesodCoreTest.Links
import YesodCoreTest.Header
import YesodCoreTest.NoOverloadedStrings
import YesodCoreTest.SubSub
import YesodCoreTest.InternalRequest
import YesodCoreTest.ErrorHandling
import YesodCoreTest.Cache
import YesodCoreTest.ParameterizedSite
import YesodCoreTest.Breadcrumb
import qualified YesodCoreTest.WaiSubsite as WaiSubsite
import qualified YesodCoreTest.Redirect as Redirect
import qualified YesodCoreTest.JsAttributes as JsAttributes
import qualified YesodCoreTest.JsLoader as JsLoader
import qualified YesodCoreTest.RequestBodySize as RequestBodySize
import qualified YesodCoreTest.Json as Json
import qualified YesodCoreTest.Content as Content
import qualified YesodCoreTest.NestedDispatch as NestedDispatch
import qualified YesodCoreTest.FallthroughDispatch as FallthroughDispatch
import qualified YesodCoreTest.RenderRouteSpec as RenderRouteSpec

-- Skip on Windows, see https://github.com/yesodweb/yesod/issues/1523#issuecomment-398278450
#ifndef WINDOWS
import qualified YesodCoreTest.RawResponse as RawResponse
#endif

import qualified YesodCoreTest.Streaming as Streaming
import qualified YesodCoreTest.Reps as Reps
import qualified YesodCoreTest.Auth as Auth
import qualified YesodCoreTest.LiteApp as LiteApp
import qualified YesodCoreTest.Ssl as Ssl
import qualified YesodCoreTest.Csrf as Csrf

import Test.Hspec

specs :: Spec
specs = do
      headerTest
      cleanPathTest
      exceptionsTest
      widgetTest
      mediaTest
      linksTest
      noOverloadedTest
      subSubTest
      internalRequestTest
      errorHandlingTest
      cacheTest
      parameterizedSiteTest
      WaiSubsite.specs
      Redirect.specs
      JsAttributes.specs
      JsLoader.specs
      RequestBodySize.specs
      Json.specs
#ifndef WINDOWS
      RawResponse.specs
#endif
      Streaming.specs
      Reps.specs
      Auth.specs
      LiteApp.specs
      Ssl.unsecSpec
      Ssl.sslOnlySpec
      Ssl.sameSiteSpec
      Csrf.csrfSpec
      breadcrumbTest
      metaTest
      Content.specs
      describe "NestedDispatch" $ do
          NestedDispatch.specs
      describe "FallthroughDispatch" $ do
          FallthroughDispatch.spec
      describe "RenderRoute" $ do
          RenderRouteSpec.spec
