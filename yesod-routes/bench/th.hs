{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, TupleSections, ViewPatterns #-}
import Yesod.Routes.TH
import Yesod.Routes.Parse
import THHelper
import Language.Haskell.TH.Syntax
import Criterion.Main
import Data.Text (words)
import Prelude hiding (words)
import Control.DeepSeq
import Yesod.Routes.TH.Simple
import Test.Hspec
import Control.Monad (forM_, unless)

$(do
    let (cons, decs) = mkRouteCons $ map (fmap parseType) resources
    clause1 <- mkDispatchClause settings resources
    clause2 <- mkSimpleDispatchClause settings resources
    return $ concat
        [ [FunD (mkName "dispatch1") [clause1]]
        , [FunD (mkName "dispatch2") [clause2]]
        , decs
        , [DataD [] (mkName "Route") [] cons [''Show, ''Eq]]
        ]
    )

instance NFData Route where
    rnf HomeR = ()
    rnf FooR = ()
    rnf (BarR i) = i `seq` ()
    rnf BazR = ()

getHomeR :: Maybe Int
getHomeR = Just 1

getFooR :: Maybe Int
getFooR = Just 2

getBarR :: Int -> Maybe Int
getBarR i = Just (i + 3)

getBazR :: Maybe Int
getBazR = Just 4

samples = take 10000 $ cycle
    [ words "foo"
    , words "foo bar"
    , words ""
    , words "bar baz"
    , words "bar 4"
    , words "bar 1234566789"
    , words "baz"
    , words "baz 4"
    , words "something else"
    ]

dispatch2a = dispatch2 `asTypeOf` dispatch1

main :: IO ()
main = do
    forM_ samples $ \sample ->
        unless (dispatch1 True (sample, "GET") == dispatch2a True (sample, "GET"))
            (error $ show sample)
    defaultMain
        [ bench "dispatch1" $ nf (map (dispatch1 True . (, "GET"))) samples
        , bench "dispatch2" $ nf (map (dispatch2a True . (, "GET"))) samples
        , bench "dispatch1a" $ nf (map (dispatch1 True . (, "GET"))) samples
        , bench "dispatch2a" $ nf (map (dispatch2a True . (, "GET"))) samples
        ]
