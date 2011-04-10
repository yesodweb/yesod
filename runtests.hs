import Test.Framework (defaultMain)
import Test.CleanPath
import Test.Exceptions
import Test.Widget
import Test.Media
import Test.Links

main :: IO ()
main = defaultMain
    [ cleanPathTest
    , exceptionsTest
    , widgetTest
    , mediaTest
    , linksTest
    ]
