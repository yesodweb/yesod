import Test.Hspec

import Test.CleanPath
import Test.Exceptions
import Test.Widget
import Test.Media
import Test.Links
import Test.NoOverloadedStrings

main :: IO ()
main = hspecX $ descriptions $ 
    [ cleanPathTest
    , exceptionsTest
    , widgetTest
    , mediaTest
    , linksTest
    , noOverloadedTest
    ]
