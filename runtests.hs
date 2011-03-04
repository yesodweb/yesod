import Test.Framework (defaultMain)
import Test.CleanPath
import Test.Exceptions

main :: IO ()
main = defaultMain
    [ cleanPathTest
    , exceptionsTest
    ]
