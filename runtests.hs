import Test.Framework (defaultMain)

import qualified Yesod.Response
import qualified Yesod.Utils
import qualified Yesod.Resource
import qualified Data.Object.Html

main :: IO ()
main = defaultMain
    [ Yesod.Response.testSuite
    , Yesod.Utils.testSuite
    , Yesod.Resource.testSuite
    , Data.Object.Html.testSuite
    ]
