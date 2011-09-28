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

twoStatics :: Dispatch Dummy Dummy Int
twoStatics = toDispatch
    [ RouteHandler [StaticPiece "foo"] False $ result $ const $ Just 2
    , RouteHandler [StaticPiece "bar"] False $ result $ const $ Just 3
    ]

multi :: Dispatch Dummy Dummy Int
multi = toDispatch
    [ RouteHandler [StaticPiece "foo"] False $ result $ const $ Just 4
    , RouteHandler [StaticPiece "bar"] True $ result $ const $ Just 5
    ]

test :: Dispatch Dummy Dummy Int -> [Text] -> Maybe Int
test dispatch ts = dispatch Dummy Nothing ts Dummy id

main :: IO ()
main = hspecX $ do
    describe "justRoot" $ do
        it "dispatches correctly" $ test justRoot [] @?= Just 1
        it "fails correctly" $ test justRoot ["foo"] @?= Nothing
    describe "twoStatics" $ do
        it "dispatches correctly to foo" $ test twoStatics ["foo"] @?= Just 2
        it "dispatches correctly to bar" $ test twoStatics ["bar"] @?= Just 3
        it "fails correctly (1)" $ test twoStatics [] @?= Nothing
        it "fails correctly (2)" $ test twoStatics ["bar", "baz"] @?= Nothing
    describe "multi" $ do
        it "dispatches correctly to foo" $ test multi ["foo"] @?= Just 4
        it "dispatches correctly to bar" $ test multi ["bar"] @?= Just 5
        it "dispatches correctly to bar/baz" $ test multi ["bar", "baz"] @?= Just 5
        it "fails correctly (1)" $ test multi [] @?= Nothing
        it "fails correctly (2)" $ test multi ["foo", "baz"] @?= Nothing
