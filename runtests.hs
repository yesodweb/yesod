import Test.Framework (defaultMain)

import qualified Yesod.Response
import qualified Yesod.Request
-- FIXME import qualified Test.Errors
-- FIXME import qualified Test.QuasiResource
import qualified Web.Mime
import qualified Yesod.Json

main :: IO ()
main = defaultMain
    [ Yesod.Response.testSuite
    , Yesod.Request.testSuite
    -- FIXME , Test.Errors.testSuite
    -- FIXME, Test.QuasiResource.testSuite
    , Web.Mime.testSuite
    , Yesod.Json.testSuite
    ]
