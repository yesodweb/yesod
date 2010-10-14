import Test.Framework (defaultMain)

import qualified Yesod.Content
import qualified Yesod.Json
import qualified Yesod.Dispatch
import qualified Yesod.Helpers.Static
import qualified Yesod.Yesod
import qualified Yesod.Handler

main :: IO ()
main = defaultMain
    [ Yesod.Content.testSuite
    , Yesod.Json.testSuite
    , Yesod.Dispatch.testSuite
    , Yesod.Helpers.Static.testSuite
    , Yesod.Yesod.testSuite
    , Yesod.Handler.testSuite
    ]
