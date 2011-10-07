module Yesod.Routes
    ( Piece (..)
    , RouteHandler (..)
    , toDispatch
    , Dispatch
    ) where

import Data.Text (Text)
import Web.ClientSession (Key)
import Yesod.Core (Route)
import qualified Data.Vector as V
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Map as Map
import Data.List (sortBy)
import Data.Ord (comparing)
import Control.Arrow (second)

data Piece = StaticPiece Text | SinglePiece

data RouteHandler sub master res = RouteHandler
    { rhPieces :: [Piece]
    , rhHasMulti :: Bool
    , rhHandler :: Dispatch sub master res
    }

type Dispatch sub master res = sub -> Maybe Key -> [Text] -> master -> (Route sub -> Route master) -> Maybe res

toDispatch :: [RouteHandler sub master res] -> Dispatch sub master res
toDispatch rhs =
    bcToDispatch bc
  where
    bc = toBC rhs

bcToDispatch :: ByCount sub master res -> Dispatch sub master res
bcToDispatch (ByCount vec rest) sub mkey ts master toMaster =
    go (\x -> x sub mkey ts master toMaster) ts pm
  where
    --pm :: PieceMap sub master res
    pm = fromMaybe rest $ vec V.!? length ts

go :: (Dispatch sub master res -> Maybe res)
   -> [Text]
   -> PieceMap sub master res
   -> Maybe res
go runDispatch _ (PieceMapEnd r) =
    firstJust runDispatch $ map snd $ sortBy (comparing fst) r
go runDispatch (t:ts) (PieceMap dyn sta) = go runDispatch ts $
    case Map.lookup t sta of
        Nothing -> dyn
        Just pm -> append dyn pm
go _ [] _ = Nothing

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust _ [] = Nothing
firstJust f (a:as) = maybe (firstJust f as) Just $ f a

append :: PieceMap a b c -> PieceMap a b c -> PieceMap a b c
append (PieceMapEnd a) (PieceMapEnd b) = PieceMapEnd $ a ++ b
append (PieceMap a x) (PieceMap b y) =
    PieceMap (append a b) (Map.unionWith append x y)
-- I'm sure there's some nice type-level trickery we could employ here somehow
-- to ensure this never happens.
append _ _ = error "Mismatched PieceMaps for append"

data PieceMap sub master res = PieceMap
    { pmDynamic :: PieceMap sub master res
    , pmStatic :: Map.Map Text (PieceMap sub master res)
    } | PieceMapEnd [(Int, Dispatch sub master res)]

toPieceMap :: Int -> [RouteHandler sub master res] -> PieceMap sub master res
toPieceMap depth = toPieceMap' depth . zip [1..]

toPieceMap' :: Int
            -> [(Int, RouteHandler sub master res)]
            -> PieceMap sub master res
toPieceMap' 0 rhs =
    PieceMapEnd $ map (second rhHandler)
                $ sortBy (comparing fst) rhs
toPieceMap' depth rhs = PieceMap
    { pmDynamic = toPieceMap' depth' dynamics
    , pmStatic = Map.map (toPieceMap' depth') statics
    }
  where
    depth' = depth - 1

    pairs = map toPair rhs
    toPair (i, RouteHandler (p:ps) b c) = (p, (i, RouteHandler ps b c))
    -- if we have no more pieces, that means this is a rhHasMulti, so fill in
    -- with dynamic
    toPair (i, RouteHandler [] b c) = (SinglePiece, (i, RouteHandler [] b c))

    getDynamic (SinglePiece, rh) = Just rh
    getDynamic _ = Nothing
    dynamics = mapMaybe getDynamic pairs

    getStatic (StaticPiece t, rh) = Just $ Map.singleton t [rh]
    getStatic _ = Nothing
    statics = Map.unionsWith (++) $ mapMaybe getStatic pairs

data ByCount sub master res = ByCount
    { bcVector :: !(V.Vector (PieceMap sub master res))
    , bcRest :: !(PieceMap sub master res)
    }

toBC :: [RouteHandler sub master res] -> ByCount sub master res
toBC rhs =
    ByCount
        { bcVector = V.map (\i -> toPieceMap i $ filter (canHaveLength i) rhs)
                   $ V.enumFromN 0 (maxLen + 1)
        , bcRest = toPieceMap maxLen $ filter rhHasMulti rhs
        }
  where
    maxLen = maximum $ map (length . rhPieces) rhs

    canHaveLength i rh =
        len == i || (len < i && rhHasMulti rh)
      where
        len = length $ rhPieces rh
