import Test.Framework (defaultMain)
import Test.CleanPath
import Test.Exceptions
import Test.Widget
import Test.Media

main :: IO ()
main = defaultMain
    [ cleanPathTest
    , exceptionsTest
    , widgetTest
    , mediaTest
    ]
