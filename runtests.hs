import Test.Framework (defaultMain)

import qualified Web.Mime
import qualified Yesod.Json

main :: IO ()
main = defaultMain
    [ Web.Mime.testSuite
    , Yesod.Json.testSuite
    ]
