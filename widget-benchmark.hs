-- | BigTable benchmark implemented using Hamlet.
--
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Criterion.Main
import Text.Hamlet
import Numeric (showInt)
import qualified Data.ByteString.Lazy as L
import qualified Text.Blaze.Renderer.Utf8 as Utf8
import Data.Monoid (mconcat)
import Text.Blaze.Html5 (table, tr, td)
import Yesod.Widget
import Control.Monad.Trans.Writer
import Control.Monad.Trans.RWS
import Data.Functor.Identity
import Yesod.Internal

main = defaultMain
    [ bench "bigTable html" $ nf bigTableHtml bigTableData
    , bench "bigTable hamlet" $ nf bigTableHamlet bigTableData
    , bench "bigTable widget" $ nf bigTableWidget bigTableData
    , bench "bigTable blaze" $ nf bigTableBlaze bigTableData
    ]
  where
    rows :: Int
    rows = 1000

    bigTableData :: [[Int]]
    bigTableData = replicate rows [1..10]
    {-# NOINLINE bigTableData #-}

bigTableHtml rows = L.length $ renderHtml [$hamlet|
<table
    $forall row <- rows
        <tr
            $forall cell <- row
                <td>#{show cell}
|]

bigTableHamlet rows = L.length $ renderHamlet id [$hamlet|
<table
    $forall row <- rows
        <tr
            $forall cell <- row
                <td>#{show cell}
|]

bigTableWidget rows = L.length $ renderHtml $ (run [$hamlet|
<table
    $forall row <- rows
        <tr
            $forall cell <- row
                <td>#{show cell}
|]) (\_ _ -> "foo")
  where
  run (GWidget w) =
    let (_, _, GWData (Body x) _ _ _ _ _ _) = runRWS w () 0
     in x
  {-
  run (GWidget w) = runIdentity $ do
    w' <- flip evalStateT 0
        $ runWriterT $ runWriterT $ runWriterT $ runWriterT
        $ runWriterT $ runWriterT $ runWriterT w
    let ((((((((),
         Body body),
         _),
         _),
         _),
         _),
         _),
         _) = w'

    return body
    -}

bigTableBlaze t = L.length $ renderHtml $ table $ mconcat $ map row t
  where
    row r = tr $ mconcat $ map (td . string . show) r
