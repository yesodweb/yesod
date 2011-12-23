import Test.Hspec
import qualified YesodCoreTest

main :: IO ()
main = hspecX $ descriptions $ YesodCoreTest.specs
