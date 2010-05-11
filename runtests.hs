import Test.Framework (defaultMain)

import qualified Yesod.Content
import qualified Yesod.Json

main :: IO ()
main = defaultMain
    [ Yesod.Content.testSuite
    , Yesod.Json.testSuite
    ]
