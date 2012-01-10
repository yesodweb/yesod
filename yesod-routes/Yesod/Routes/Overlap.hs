-- | Check for overlapping routes.
module Yesod.Routes.Overlap
    ( findOverlaps
    , findOverlapNames
    ) where

import Yesod.Routes.TH.Types
import Control.Arrow ((***))

findOverlaps :: [Resource t] -> [(Resource t, Resource t)]
findOverlaps = undefined

findOverlapNames :: [Resource t] -> [(String, String)]
findOverlapNames = map (resourceName *** resourceName) . findOverlaps

{-
-- n^2, should be a way to speed it up
findOverlaps :: [Resource a] -> [[Resource a]]
findOverlaps = go . map justPieces
  where
    justPieces :: Resource a -> ([Piece a], Resource a)
    justPieces r@(Resource _ ps _) = (ps, r)

    go [] = []
    go (x:xs) = mapMaybe (mOverlap x) xs ++ go xs

    mOverlap :: ([Piece a], Resource a) -> ([Piece a], Resource a) ->
                Maybe [Resource a]
    mOverlap _ _ = Nothing
                {- FIXME mOverlap
    mOverlap (Static x:xs, xr) (Static y:ys, yr)
        | x == y = mOverlap (xs, xr) (ys, yr)
        | otherwise = Nothing
    mOverlap (MultiPiece _:_, xr) (_, yr) = Just (xr, yr)
    mOverlap (_, xr) (MultiPiece _:_, yr) = Just (xr, yr)
    mOverlap ([], xr) ([], yr) = Just (xr, yr)
    mOverlap ([], _) (_, _) = Nothing
    mOverlap (_, _) ([], _) = Nothing
    mOverlap (_:xs, xr) (_:ys, yr) = mOverlap (xs, xr) (ys, yr)
    -}
-}
