import Test.Framework (defaultMain)

import qualified Web.Restful.Response
import qualified Web.Restful.Utils
import qualified Web.Restful.Resource

main :: IO ()
main = defaultMain
    [ Web.Restful.Response.testSuite
    , Web.Restful.Utils.testSuite
    , Web.Restful.Resource.testSuite
    ]
