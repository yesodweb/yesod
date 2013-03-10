module YesodCoreTest (specs) where 

import YesodCoreTest.CleanPath
import YesodCoreTest.Exceptions
import YesodCoreTest.Widget
import YesodCoreTest.Media
import YesodCoreTest.Links
import YesodCoreTest.NoOverloadedStrings
import YesodCoreTest.InternalRequest
import YesodCoreTest.ErrorHandling
import YesodCoreTest.Cache
import qualified YesodCoreTest.WaiSubsite as WaiSubsite
import qualified YesodCoreTest.Redirect as Redirect
import qualified YesodCoreTest.JsLoader as JsLoader
import qualified YesodCoreTest.RequestBodySize as RequestBodySize
import qualified YesodCoreTest.Json as Json
import qualified YesodCoreTest.Auth as Auth

import Test.Hspec

specs :: Spec
specs = do
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
      RequestBodySize.specs
      Json.specs
      Auth.specs
