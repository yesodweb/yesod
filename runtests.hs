import Test.Framework (defaultMain)

-- FIXME import qualified Test.Errors
-- FIXME import qualified Test.QuasiResource
import qualified Web.Mime
import qualified Yesod.Json

main :: IO ()
main = defaultMain
    [
      Web.Mime.testSuite
    , Yesod.Json.testSuite
    -- FIXME , Test.Errors.testSuite
    -- FIXME, Test.QuasiResource.testSuite
    ]
