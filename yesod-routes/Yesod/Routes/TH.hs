{-# LANGUAGE TemplateHaskell #-}
module Yesod.Routes.TH
    ( -- * Data types
      Resource (..)
    , Piece (..)
    , Dispatch (..)
      -- * Functions
      -- ** Route data type
    , mkRouteType
    , mkRouteCons
      -- ** RenderRoute
    , mkRenderRouteClauses
    , mkRenderRouteInstance
      -- ** Dispatch
    , mkDispatchClause
    ) where

import Language.Haskell.TH.Syntax
import Yesod.Core
    ( Route, RenderRoute (renderRoute), toSinglePiece, toMultiPiece
    )
import Data.Maybe (maybeToList)
import Control.Monad (replicateM)
import Data.Text (pack)
import qualified Yesod.Routes.Dispatch as D

data Resource = Resource
    { resourceName :: String
    , resourcePieces :: [Piece]
    , resourceMulti :: Maybe Type
    , resourceDispatch :: Dispatch
    }

data Piece = Static String | Dynamic Type

data Dispatch = AllMethods | Methods [String] | Subsite
    { subsiteType :: Type
    , subsiteFunc :: String
    }

mkRouteCons :: [Resource] -> [Con]
mkRouteCons =
    map mkRouteCon
  where
    mkRouteCon res =
        NormalC (mkName $ resourceName res)
            $ map (\x -> (NotStrict, x))
            $ concat [singles, multi, sub]
      where
        singles = concatMap toSingle $ resourcePieces res
        toSingle Static{} = []
        toSingle (Dynamic typ) = [typ]

        multi = maybeToList $ resourceMulti res

        sub =
            case resourceDispatch res of
                Subsite { subsiteType = typ } -> [ConT ''Route `AppT` typ]
                _ -> []

mkRouteType :: String -> [Resource] -> Dec
mkRouteType name res =
    DataD [] (mkName name) [] (mkRouteCons res) clazzes
  where
    clazzes = [''Show, ''Eq, ''Read]

mkRenderRouteClauses :: [Resource] -> Q [Clause]
mkRenderRouteClauses =
    mapM go
  where
    isDynamic Dynamic{} = True
    isDynamic _ = False

    go res = do
        let cnt = length (filter isDynamic $ resourcePieces res) + maybe 0 (const 1) (resourceMulti res)
        dyns <- replicateM cnt $ newName "dyn"
        sub <-
            case resourceDispatch res of
                Subsite{} -> fmap return $ newName "sub"
                _ -> return []
        let pat = ConP (mkName $ resourceName res) $ map VarP $ dyns ++ sub

        pack <- [|pack|]
        tsp <- [|toSinglePiece|]
        let piecesSingle = mkPieces (AppE pack . LitE . StringL) tsp (resourcePieces res) dyns

        piecesMulti <-
            case resourceMulti res of
                Nothing -> return $ ListE []
                Just{} -> do
                    tmp <- [|toMultiPiece|]
                    return $ tmp `AppE` VarE (last dyns)

        body <-
            case sub of
                [x] -> do
                    rr <- [|renderRoute|]
                    a <- newName "a"
                    b <- newName "b"

                    colon <- [|(:)|]
                    let cons a b = InfixE (Just a) colon (Just b)
                    let pieces = foldr cons (VarE a) piecesSingle

                    return $ LamE [TupP [VarP a, VarP b]] (TupE [pieces, VarE b]) `AppE` (rr `AppE` VarE x)
                _ -> do
                    colon <- [|(:)|]
                    let cons a b = InfixE (Just a) colon (Just b)
                    return $ TupE [foldr cons piecesMulti piecesSingle, ListE []]

        return $ Clause [pat] (NormalB body) []

    mkPieces _ _ [] _ = []
    mkPieces toText tsp (Static s:ps) dyns = toText s : mkPieces toText tsp ps dyns
    mkPieces toText tsp (Dynamic{}:ps) (d:dyns) = tsp `AppE` VarE d : mkPieces toText tsp ps dyns

mkRenderRouteInstance :: String -> [Resource] -> Q Dec
mkRenderRouteInstance name ress = do
    cls <- mkRenderRouteClauses ress
    return $ InstanceD [] (ConT ''RenderRoute `AppT` ConT (mkName name))
        [ FunD (mkName "renderRoute") cls
        ]

mkDispatchClause :: [Resource] -> Q Clause
mkDispatchClause ress = do
    let routes = fmap ListE $ mapM toRoute ress
    sub <- newName "sub"
    mkey <- newName "mkey"
    ts <- newName "ts"
    master <- newName "master"
    toMaster <- newName "toMaster"
    let pats =
            [ VarP sub
            , VarP mkey
            , VarP ts
            , VarP master
            , VarP toMaster
            ]

    dispatch <- newName "dispatch"
    body <- [|D.toDispatch $(routes)|]
    return $ Clause
        pats
        (NormalB $ VarE dispatch `AppE` VarE ts `AppE` TupE (map VarE [sub, mkey, master, toMaster]))
        [FunD dispatch [Clause [] (NormalB body) []]]
  where
    toRoute :: Resource -> Q Exp
    toRoute res = do
        let ps = fmap ListE $ mapM toPiece $ resourcePieces res
        let m = maybe [|False|] (const [|True|]) $ resourceMulti res
        [|D.Route $(ps) $(m) undefined|]

    toPiece :: Piece -> Q Exp
    toPiece (Static s) = [|D.Static $ pack $(lift s)|]
    toPiece Dynamic{} = [|D.Dynamic|]
