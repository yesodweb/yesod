import Test.Framework (defaultMain)

import qualified Yesod.Response
import qualified Yesod.Resource
import qualified Yesod.Request
import qualified Data.Object.Html
import qualified Test.Errors
import qualified Test.QuasiResource
import qualified Web.Mime

main :: IO ()
main = defaultMain
    [ Yesod.Response.testSuite
    , Yesod.Resource.testSuite
    , Yesod.Request.testSuite
    , Data.Object.Html.testSuite
    , Test.Errors.testSuite
    , Test.QuasiResource.testSuite
    , Web.Mime.testSuite
    ]
