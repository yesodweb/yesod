import Test.Hspec

import Test.CleanPath
import Test.Exceptions
import Test.Widget
import Test.Media
import Test.Links
import Test.NoOverloadedStrings
import Test.InternalRequest
import Test.ErrorHandling

main :: IO ()
main = hspecX $ descriptions $
    [ cleanPathTest
    , exceptionsTest
    , widgetTest
    , mediaTest
    , linksTest
    , noOverloadedTest
    , internalRequestTest
    , errorHandlingTest
    ]
