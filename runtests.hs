import Test.Framework (defaultMain)

import qualified Yesod.Content
import qualified Yesod.Json
import qualified Yesod.Dispatch
import qualified Yesod.Helpers.Static

main :: IO ()
main = defaultMain
    [ Yesod.Content.testSuite
    , Yesod.Json.testSuite
    , Yesod.Dispatch.testSuite
    , Yesod.Helpers.Static.testSuite
    ]
