-- | Check for overlapping routes.
module Yesod.Routes.Overlap
    ( findOverlaps
    , findOverlapNames
    ) where

import Yesod.Routes.TH.Types
import Control.Arrow ((***))
import Data.Maybe (mapMaybe)

findOverlaps :: [Resource t] -> [(Resource t, Resource t)]
findOverlaps [] = []
findOverlaps (x:xs) = mapMaybe (findOverlap x) xs ++ findOverlaps xs

findOverlap :: Resource t -> Resource t -> Maybe (Resource t, Resource t)
findOverlap x y
    | overlaps (resourcePieces x) (resourcePieces y) (hasSuffix x) (hasSuffix y) = Just (x, y)
    | otherwise = Nothing

hasSuffix :: Resource t -> Bool
hasSuffix r =
    case resourceDispatch r of
        Subsite{} -> True
        Methods Just{} _ -> True
        Methods Nothing _ -> False

overlaps :: [(CheckOverlap, Piece t)] -> [(CheckOverlap, Piece t)] -> Bool -> Bool -> Bool
overlaps [] [] False False = False
overlaps [] _ suffixX _ = suffixX
overlaps _ [] _ suffixY = suffixY
overlaps ((False, _):xs) (_:ys) suffixX suffixY = overlaps xs ys suffixX suffixY
overlaps (_:xs) ((False, _):ys) suffixX suffixY = overlaps xs ys suffixX suffixY
overlaps ((True, pieceX):xs) ((True, pieceY):ys) suffixX suffixY =
    piecesOverlap pieceX pieceY || overlaps xs ys suffixX suffixY

piecesOverlap :: Piece t -> Piece t -> Bool
piecesOverlap (Static x) (Static y) = x == y
piecesOverlap _ _ = True

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
