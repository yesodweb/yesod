import Test.Framework (defaultMain)

import Yesod.Content
import Yesod.Dispatch
import Yesod.Handler

main :: IO ()
main = defaultMain
    [ contentTestSuite
    , dispatchTestSuite
    , handlerTestSuite
    ]
