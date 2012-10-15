-- | Check for overlapping routes.
module Yesod.Routes.Overlap
    ( findOverlaps
    , findOverlapNames
    , Overlap (..)
    ) where

import Yesod.Routes.TH.Types
import Data.List (intercalate)

data Overlap t = Overlap
    { overlapParents :: [String] -> [String] -- ^ parent resource trees
    , overlap1 :: ResourceTree t
    , overlap2 :: ResourceTree t
    }

findOverlaps :: ([String] -> [String]) -> [ResourceTree t] -> [Overlap t]
findOverlaps _ [] = []
findOverlaps front (x:xs) = concatMap (findOverlap front x) xs ++ findOverlaps front xs

findOverlap :: ([String] -> [String]) -> ResourceTree t -> ResourceTree t -> [Overlap t]
findOverlap front x y =
    here rest
  where
    here
        | overlaps (resourceTreePieces x) (resourceTreePieces y) (hasSuffix x) (hasSuffix y) = (Overlap front x y:)
        | otherwise = id
    rest =
        case x of
            ResourceParent name _ children -> findOverlaps (front . (name:)) children
            ResourceLeaf{} -> []

hasSuffix :: ResourceTree t -> Bool
hasSuffix (ResourceLeaf r) =
    case resourceDispatch r of
        Subsite{} -> True
        Methods Just{} _ -> True
        Methods Nothing _ -> False
hasSuffix ResourceParent{} = True

overlaps :: [(CheckOverlap, Piece t)] -> [(CheckOverlap, Piece t)] -> Bool -> Bool -> Bool

-- No pieces on either side, will overlap regardless of suffix
overlaps [] [] _ _ = True

-- No pieces on the left, will overlap if the left side has a suffix
overlaps [] _ suffixX _ = suffixX

-- Ditto for the right
overlaps _ [] _ suffixY = suffixY

-- As soon as we ignore a single piece (via CheckOverlap == False), we say that
-- the routes don't overlap at all. In other words, disabling overlap checking
-- on a single piece disables it on the whole route.
overlaps ((False, _):_) _ _ _ = False
overlaps _ ((False, _):_) _ _ = False

-- Compare the actual pieces
overlaps ((True, pieceX):xs) ((True, pieceY):ys) suffixX suffixY =
    piecesOverlap pieceX pieceY && overlaps xs ys suffixX suffixY

piecesOverlap :: Piece t -> Piece t -> Bool
-- Statics only match if they equal. Dynamics match with anything
piecesOverlap (Static x) (Static y) = x == y
piecesOverlap _ _ = True

findOverlapNames :: [ResourceTree t] -> [(String, String)]
findOverlapNames =
    map go . findOverlaps id
  where
    go (Overlap front x y) =
        (go' $ resourceTreeName x, go' $ resourceTreeName y)
      where
        go' = intercalate "/" . front . return
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
