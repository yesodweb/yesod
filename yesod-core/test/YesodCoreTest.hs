{-# LANGUAGE CPP                        #-}
module YesodCoreTest (specs) where

import YesodCoreTest.CleanPath
import YesodCoreTest.Exceptions
import YesodCoreTest.Widget
import YesodCoreTest.Media
import YesodCoreTest.Links
import YesodCoreTest.Header
import YesodCoreTest.NoOverloadedStrings
import YesodCoreTest.InternalRequest
import YesodCoreTest.ErrorHandling
import YesodCoreTest.Cache
import qualified YesodCoreTest.WaiSubsite as WaiSubsite
import qualified YesodCoreTest.Redirect as Redirect
import qualified YesodCoreTest.JsLoader as JsLoader
import qualified YesodCoreTest.JsAnchor as JsAnchor
import qualified YesodCoreTest.RequestBodySize as RequestBodySize
import qualified YesodCoreTest.Json as Json

-- Skip on Windows, see https://github.com/yesodweb/yesod/issues/1523#issuecomment-398278450
#if !WINDOWS
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
      internalRequestTest
      errorHandlingTest
      cacheTest
      WaiSubsite.specs
      Redirect.specs
      JsLoader.specs
      JsAnchor.specs
      RequestBodySize.specs
      Json.specs
#if !WINDOWS
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
