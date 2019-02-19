-- | Check for overlapping routes.
module Yesod.Routes.Overlap
    ( findOverlapNames
    , Overlap (..)
    ) where

import Yesod.Routes.TH.Types
import Data.List (intercalate)

data Flattened t = Flattened
    { fNames :: [String]
    , fPieces :: [Piece t]
    , fHasSuffix :: Bool
    , fCheck :: CheckOverlap
    }

flatten :: ResourceTree t -> [Flattened t]
flatten =
    go id id True
  where
    go names pieces check (ResourceLeaf r) = return Flattened
        { fNames = names [resourceName r]
        , fPieces = pieces (resourcePieces r)
        , fHasSuffix = hasSuffix $ ResourceLeaf r
        , fCheck = check && resourceCheck r
        }
    go names pieces check (ResourceParent newname check' newpieces children) =
        concatMap (go names' pieces' (check && check')) children
      where
        names' = names . (newname:)
        pieces' = pieces . (newpieces ++)

data Overlap t = Overlap
    { overlapParents :: [String] -> [String] -- ^ parent resource trees
    , overlap1 :: ResourceTree t
    , overlap2 :: ResourceTree t
    }

data OverlapF = OverlapF
    { _overlapF1 :: [String]
    , _overlapF2 :: [String]
    }

overlaps :: [Piece t] -> [Piece t] -> Bool -> Bool -> Bool

-- No pieces on either side, will overlap regardless of suffix
overlaps [] [] _ _ = True

-- No pieces on the left, will overlap if the left side has a suffix
overlaps [] _ suffixX _ = suffixX

-- Ditto for the right
overlaps _ [] _ suffixY = suffixY

-- Compare the actual pieces
overlaps (pieceX:xs) (pieceY:ys) suffixX suffixY =
    piecesOverlap pieceX pieceY && overlaps xs ys suffixX suffixY

piecesOverlap :: Piece t -> Piece t -> Bool
-- Statics only match if they equal. Dynamics match with anything
piecesOverlap (Static x) (Static y) = x == y
piecesOverlap _ _ = True

findOverlapNames :: [ResourceTree t] -> [(String, String)]
findOverlapNames =
    map go . findOverlapsF . filter fCheck . concatMap Yesod.Routes.Overlap.flatten
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
