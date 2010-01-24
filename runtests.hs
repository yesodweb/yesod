import Test.Framework (defaultMain)

import qualified Yesod.Response
import qualified Yesod.Resource
import qualified Yesod.Rep
import qualified Yesod.Request
import qualified Data.Object.Html
import qualified Test.Errors
import qualified Test.QuasiResource

main :: IO ()
main = defaultMain
    [ Yesod.Response.testSuite
    , Yesod.Resource.testSuite
    , Yesod.Rep.testSuite
    , Yesod.Request.testSuite
    , Data.Object.Html.testSuite
    , Test.Errors.testSuite
    , Test.QuasiResource.testSuite
    ]
