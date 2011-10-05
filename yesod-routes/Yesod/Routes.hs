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
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

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
    go rhs
  where
    len = length ts
    rhs = fromMaybe rest $ vec V.!? len

    go [] = Nothing
    go (x:xs) = maybe (go xs) Just $ if checkStatics ts (rhPieces x) (rhHasMulti x) then rhHandler x sub mkey ts master toMaster else Nothing

    checkStatics [] [] _ = True
    checkStatics [] _ _ = False
    checkStatics _ [] isMulti = isMulti
    checkStatics (_:paths) (SinglePiece:pieces) isMulti = checkStatics paths pieces isMulti
    checkStatics (path:paths) (StaticPiece piece:pieces) isMulti =
        path == piece && checkStatics paths pieces isMulti

data PieceMap sub master res = PieceMap
    { pmHandlers :: Either (PieceMap sub master res) [(Int, RouteHandler sub master res)]
    , pmStatic :: Map.Map Text (PieceMap sub master res)
    }

toPieceMap :: [RouteHandler sub master res] -> PieceMap sub master res
toPieceMap = undefined

data ByCount sub master res = ByCount
    { bcVector :: !(V.Vector [RouteHandler sub master res])
    , bcRest :: ![RouteHandler sub master res]
    }

toBC :: [RouteHandler sub master res] -> ByCount sub master res
toBC rhs =
    ByCount
        { bcVector = V.map (\i -> filter (canHaveLength i) rhs) $ V.enumFromN 0 (maxLen + 1)
        , bcRest = filter rhHasMulti rhs
        }
  where
    maxLen = maximum $ map (length . rhPieces) rhs

    canHaveLength i rh =
        len == i || (len < i && rhHasMulti rh)
      where
        len = length $ rhPieces rh
