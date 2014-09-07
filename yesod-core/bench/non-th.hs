{-# LANGUAGE OverloadedStrings #-}
import Yesod.Routes.Dispatch
import Data.Text (Text, words)
import Prelude hiding (words)
import Web.PathPieces
import Criterion.Main
import Control.DeepSeq
import Control.Monad (forM_, unless)

data TestRoute = Foo | Bar !Int | Baz
    deriving Eq
instance NFData TestRoute

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

simple :: [Text] -> Maybe TestRoute
simple ["foo"] = Just Foo
simple ["bar", x] = fmap Bar (fromPathPiece x)
simple ["baz"] = Just Baz
simple ["FOO"] = Just Foo
simple ["BAR", x] = fmap Bar (fromPathPiece x)
simple ["BAZ"] = Just Baz
simple ["Foo"] = Just Foo
simple ["Bar", x] = fmap Bar (fromPathPiece x)
simple ["Baz"] = Just Baz
simple ["Xfoo"] = Just Foo
simple ["Xbar", x] = fmap Bar (fromPathPiece x)
simple ["Xbaz"] = Just Baz
simple ["XFOO"] = Just Foo
simple ["XBAR", x] = fmap Bar (fromPathPiece x)
simple ["XBAZ"] = Just Baz
simple ["XFoo"] = Just Foo
simple ["XBar", x] = fmap Bar (fromPathPiece x)
simple ["XBaz"] = Just Baz
simple _ = Nothing

dispatch :: [Text] -> Maybe TestRoute
dispatch = toDispatch
    [ Route [Static "foo"] False (const (Just Foo))
    , Route [Static "bar", Dynamic] False (\[_, x] -> (fmap Bar (fromPathPiece x)))
    , Route [Static "baz"] False (const (Just Baz))
    , Route [Static "FOO"] False (const (Just Foo))
    , Route [Static "BAR", Dynamic] False (\[_, x] -> (fmap Bar (fromPathPiece x)))
    , Route [Static "BAZ"] False (const (Just Baz))
    , Route [Static "Foo"] False (const (Just Foo))
    , Route [Static "Bar", Dynamic] False (\[_, x] -> (fmap Bar (fromPathPiece x)))
    , Route [Static "Baz"] False (const (Just Baz))
    , Route [Static "Xfoo"] False (const (Just Foo))
    , Route [Static "Xbar", Dynamic] False (\[_, x] -> (fmap Bar (fromPathPiece x)))
    , Route [Static "Xbaz"] False (const (Just Baz))
    , Route [Static "XFOO"] False (const (Just Foo))
    , Route [Static "XBAR", Dynamic] False (\[_, x] -> (fmap Bar (fromPathPiece x)))
    , Route [Static "XBAZ"] False (const (Just Baz))
    , Route [Static "XFoo"] False (const (Just Foo))
    , Route [Static "XBar", Dynamic] False (\[_, x] -> (fmap Bar (fromPathPiece x)))
    , Route [Static "XBaz"] False (const (Just Baz))
    ]

main :: IO ()
main = do
    forM_ samples $ \sample -> unless (simple sample == dispatch sample) (error $ show sample)
    defaultMain
        [ bench "simple" $ nf (map simple) samples
        , bench "dispatch" $ nf (map dispatch) samples
        ]
