import Test.Hspec
import YesodShakespeareTest.Widget
import YesodShakespeareTest.Media

main :: IO ()
main = hspec specs

specs :: Spec
specs = do
    widgetTest
    mediaTest
