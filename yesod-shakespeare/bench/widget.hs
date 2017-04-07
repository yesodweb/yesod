{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
-- | BigTable benchmark implemented using Hamlet.
--
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Criterion.Main
import Yesod.Shakespeare
import Numeric (showInt)
import Text.Hamlet
import qualified Data.ByteString.Lazy as L
import qualified Text.Blaze.Html.Renderer.Utf8 as Utf8
import Data.Monoid (mconcat)
import Text.Blaze.Html5 (table, tr, td)
import Text.Blaze.Html (toHtml)
import Yesod.Core.Widget
import Yesod.Core.Types
import Data.Int

main :: IO ()
main = defaultMain
    [ bench "bigTable html" $ nf bigTableHtml bigTableData
    , bench "bigTable hamlet" $ nf bigTableHamlet bigTableData
    , bench "bigTable widget" $ nfIO (bigTableWidget bigTableData)
    , bench "bigTable blaze" $ nf bigTableBlaze bigTableData
    ]
  where
    rows :: Int
    rows = 1000

    bigTableData :: [[Int]]
    bigTableData = replicate rows [1..10]
    {-# NOINLINE bigTableData #-}

bigTableHtml :: Show a => [[a]] -> Int64
bigTableHtml rows = L.length $ Utf8.renderHtml $ ($ id) [hamlet|
<table>
    $forall row <- rows
        <tr>
            $forall cell <- row
                <td>#{show cell}
|]

bigTableHamlet :: Show a => [[a]] ->  Int64
bigTableHamlet rows = L.length $ Utf8.renderHtml $ ($ id) [hamlet|
<table>
    $forall row <- rows
        <tr>
            $forall cell <- row
                <td>#{show cell}
|]

bigTableWidget :: Show a => [[a]] -> IO Int64
bigTableWidget rows = fmap (L.length . Utf8.renderHtml . ($ render)) (run [whamlet|
<table>
    $forall row <- rows
        <tr>
            $forall cell <- row
                <td>#{show cell}
|])
  where
  render _ _ = "foo"
  run (WidgetT w) = do
    (_, GWData { gwdBody = Body x }) <- w undefined
    return x

bigTableBlaze :: Show a => [[a]] -> Int64
bigTableBlaze t = L.length $ Utf8.renderHtml $ table $ Data.Monoid.mconcat $ map row t
  where
    row r = tr $ mconcat $ map (td . toHtml . show) r
