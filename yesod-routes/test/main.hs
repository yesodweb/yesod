{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Test.HUnit ((@?=))
import Data.Text (Text, unpack)
import Yesod.Routes.Dispatch

result :: ([Text] -> Maybe Int) -> Dispatch () Int
result f ts () = f ts

justRoot :: Dispatch () Int
justRoot = toDispatch
    [ Route [] False $ result $ const $ Just 1
    ]

twoStatics :: Dispatch () Int
twoStatics = toDispatch
    [ Route [Static "foo"] False $ result $ const $ Just 2
    , Route [Static "bar"] False $ result $ const $ Just 3
    ]

multi :: Dispatch () Int
multi = toDispatch
    [ Route [Static "foo"] False $ result $ const $ Just 4
    , Route [Static "bar"] True $ result $ const $ Just 5
    ]

dynamic :: Dispatch () Int
dynamic = toDispatch
    [ Route [Static "foo"] False $ result $ const $ Just 6
    , Route [Dynamic] False $ result $ \ts ->
        case ts of
            [t] ->
                case reads $ unpack t of
                    [] -> Nothing
                    (i, _):_ -> Just i
            _ -> error $ "Called dynamic with: " ++ show ts
    ]

overlap :: Dispatch () Int
overlap = toDispatch
    [ Route [Static "foo"] False $ result $ const $ Just 20
    , Route [Static "foo"] True $ result $ const $ Just 21
    , Route [] True $ result $ const $ Just 22
    ]

test :: Dispatch () Int -> [Text] -> Maybe Int
test dispatch ts = dispatch ts ()

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
    describe "dynamic" $ do
        it "dispatches correctly to foo" $ test dynamic ["foo"] @?= Just 6
        it "dispatches correctly to 7" $ test dynamic ["7"] @?= Just 7
        it "dispatches correctly to 42" $ test dynamic ["42"] @?= Just 42
        it "fails correctly on five" $ test dynamic ["five"] @?= Nothing
        it "fails correctly on too many" $ test dynamic ["foo", "baz"] @?= Nothing
        it "fails correctly on too few" $ test dynamic [] @?= Nothing
    describe "overlap" $ do
        it "dispatches correctly to foo" $ test overlap ["foo"] @?= Just 20
        it "dispatches correctly to foo/bar" $ test overlap ["foo", "bar"] @?= Just 21
        it "dispatches correctly to bar" $ test overlap ["bar"] @?= Just 22
        it "dispatches correctly to []" $ test overlap [] @?= Just 22
