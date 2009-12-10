import Test.Framework (defaultMain)

import qualified Yesod.Response
import qualified Yesod.Utils
import qualified Yesod.Resource

main :: IO ()
main = defaultMain
    [ Yesod.Response.testSuite
    , Yesod.Utils.testSuite
    , Yesod.Resource.testSuite
    ]
