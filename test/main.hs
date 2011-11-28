import Test.Hspec
import qualified YesodCoreTest
import qualified YesodStaticTest

main :: IO ()
main = hspecX $ descriptions [
    concat YesodCoreTest.specs
  , concat YesodStaticTest.specs
  ]
