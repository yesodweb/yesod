import Test.Framework (defaultMain)
import Test.CleanPath
import Test.Exceptions
import Test.Widget

main :: IO ()
main = defaultMain
    [ cleanPathTest
    , exceptionsTest
    , widgetTest
    ]
