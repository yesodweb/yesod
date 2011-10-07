> module Yesod.Routes.Dispatch
>     ( Piece (..)
>     , RouteHandler (..)
>     , Dispatch
>     , toDispatch
>     ) where
>
> import Data.Text (Text)
> import qualified Data.Vector as V
> import Data.Maybe (fromMaybe, mapMaybe)
> import qualified Data.Map as Map
> import Data.List (sortBy)
> import Data.Ord (comparing)
> import Control.Arrow (second)
>
> data Piece = StaticPiece Text | SinglePiece
> type Dispatch req res = [Text] -> req -> Maybe res
>
> data RouteHandler req res = RouteHandler
>     { rhPieces :: [Piece]
>     , rhHasMulti :: Bool
>     , rhDispatch :: Dispatch req res
>     }
>
> toDispatch :: [RouteHandler req res] -> [Text] -> req -> Maybe res
> toDispatch rhs =
>     bcToDispatch bc
>   where
>     bc = toBC rhs
>
> bcToDispatch :: ByCount req res -> Dispatch req res
> bcToDispatch (ByCount vec rest) ts0 req =
>     bcToDispatch' ts0 pm0
>   where
>     --pm0 :: PieceMap sub master res
>     pm0 = fromMaybe rest $ vec V.!? length ts0
>
>     --bcToDispatch' :: [Text] -> PieceMap req res -> Maybe res
>     bcToDispatch' _ (PieceMapEnd r) =
>         firstJust (\f -> f ts0 req) $ map snd $ sortBy (comparing fst) r
>     bcToDispatch' (t:ts) (PieceMap dyn sta) = bcToDispatch' ts $
>         case Map.lookup t sta of
>             Nothing -> dyn
>             Just pm -> append dyn pm
>     bcToDispatch' [] _ = Nothing
>
> firstJust :: (a -> Maybe b) -> [a] -> Maybe b
> firstJust _ [] = Nothing
> firstJust f (a:as) = maybe (firstJust f as) Just $ f a
>
> append :: PieceMap a b -> PieceMap a b -> PieceMap a b
> append (PieceMapEnd a) (PieceMapEnd b) = PieceMapEnd $ a ++ b
> append (PieceMap a x) (PieceMap b y) =
>     PieceMap (append a b) (Map.unionWith append x y)
> -- I'm sure there's some nice type-level trickery we could employ here somehow
> -- to ensure this never happens.
> append _ _ = error "Mismatched PieceMaps for append"
>
> data PieceMap req res = PieceMap
>     { pmDynamic :: PieceMap req res
>     , pmStatic :: Map.Map Text (PieceMap req res)
>     } | PieceMapEnd [(Int, Dispatch req res)]
>
> toPieceMap :: Int -> [RouteHandler req res] -> PieceMap req res
> toPieceMap depth = toPieceMap' depth . zip [1..]
>
> toPieceMap' :: Int
>             -> [(Int, RouteHandler req res)]
>             -> PieceMap req res
> toPieceMap' 0 rhs =
>     PieceMapEnd $ map (second rhDispatch)
>                 $ sortBy (comparing fst) rhs
> toPieceMap' depth rhs = PieceMap
>     { pmDynamic = toPieceMap' depth' dynamics
>     , pmStatic = Map.map (toPieceMap' depth') statics
>     }
>   where
>     depth' = depth - 1
>
>     pairs = map toPair rhs
>     toPair (i, RouteHandler (p:ps) b c) = (p, (i, RouteHandler ps b c))
>     -- if we have no more pieces, that means this is a rhHasMulti, so fill in
>     -- with dynamic
>     toPair (i, RouteHandler [] b c) = (SinglePiece, (i, RouteHandler [] b c))
>
>     getDynamic (SinglePiece, rh) = Just rh
>     getDynamic _ = Nothing
>     dynamics = mapMaybe getDynamic pairs
>
>     getStatic (StaticPiece t, rh) = Just $ Map.singleton t [rh]
>     getStatic _ = Nothing
>     statics = Map.unionsWith (++) $ mapMaybe getStatic pairs
>
> data ByCount req res = ByCount
>     { bcVector :: !(V.Vector (PieceMap req res))
>     , bcRest :: !(PieceMap req res)
>     }
>
> toBC :: [RouteHandler req res] -> ByCount req res
> toBC rhs =
>     ByCount
>         { bcVector = V.map (\i -> toPieceMap i $ filter (canHaveLength i) rhs)
>                    $ V.enumFromN 0 (maxLen + 1)
>         , bcRest = toPieceMap maxLen $ filter rhHasMulti rhs
>         }
>   where
>     maxLen = maximum $ map (length . rhPieces) rhs
>
>     canHaveLength i rh =
>         len == i || (len < i && rhHasMulti rh)
>       where
>         len = length $ rhPieces rh
