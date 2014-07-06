-- | Check for overlapping routes.
module Yesod.Routes.Overlap
    ( findOverlaps
    , findOverlapNames
    , Overlap (..)
    ) where

import Yesod.Routes.TH.Types
import Data.List (intercalate)

data Flattened t = Flattened
    { fNames :: [String]
    , fPieces :: [(CheckOverlap, Piece t)]
    , fHasSuffix :: Bool
    }

flatten :: ResourceTree t -> [Flattened t]
flatten =
    go id id
  where
    go names pieces (ResourceLeaf r) = return Flattened
        { fNames = names [resourceName r]
        , fPieces = pieces (resourcePieces r)
        , fHasSuffix = hasSuffix $ ResourceLeaf r
        }
    go names pieces (ResourceParent newname newpieces children) =
        concatMap (go names' pieces') children
      where
        names' = names . (newname:)
        pieces' = pieces . (newpieces ++)

data Overlap t = Overlap
    { overlapParents :: [String] -> [String] -- ^ parent resource trees
    , overlap1 :: ResourceTree t
    , overlap2 :: ResourceTree t
    }

data OverlapF = OverlapF
    { overlapF1 :: [String]
    , overlapF2 :: [String]
    }

findOverlaps :: ([String] -> [String]) -> [ResourceTree t] -> [Overlap t]
findOverlaps _ [] = []
findOverlaps front (x:xs) = concatMap (findOverlap front x) xs ++ findOverlaps front xs
{-# DEPRECATED findOverlaps "This function is no longer used" #-}

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
    map go . findOverlapsF . concatMap Yesod.Routes.Overlap.flatten
  where
    go (OverlapF x y) =
        (go' x, go' y)
      where
        go' = intercalate "/"

findOverlapsF :: [Flattened t] -> [OverlapF]
findOverlapsF [] = []
findOverlapsF (x:xs) = concatMap (findOverlapF x) xs ++ findOverlapsF xs

findOverlapF :: Flattened t -> Flattened t -> [OverlapF]
findOverlapF x y
    | overlaps (fPieces x) (fPieces y) (fHasSuffix x) (fHasSuffix y) = [OverlapF (fNames x) (fNames y)]
    | otherwise = []

hasSuffix :: ResourceTree t -> Bool
hasSuffix (ResourceLeaf r) =
    case resourceDispatch r of
        Subsite{} -> True
        Methods Just{} _ -> True
        Methods Nothing _ -> False
hasSuffix ResourceParent{} = True
