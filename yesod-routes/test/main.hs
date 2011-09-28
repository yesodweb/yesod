{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Test.HUnit ((@?=))
import Data.Text (Text)
import Yesod.Routes

data Dummy = Dummy

result :: ([Text] -> Maybe Int) -> Dispatch sub master Int
result f _ _ ts _ _ = f ts

justRoot :: Dispatch Dummy Dummy Int
justRoot = toDispatch
    [ RouteHandler [] False $ result $ const $ Just 1
    ]

test :: Dispatch Dummy Dummy Int -> [Text] -> Maybe Int
test dispatch ts = dispatch Dummy Nothing ts Dummy id

main :: IO ()
main = hspecX $ do
    describe "justRoot" $ do
        it "dispatches correctly" $ test justRoot [] @?= Just 1
        it "fails correctly" $ test justRoot ["foo"] @?= Nothing
