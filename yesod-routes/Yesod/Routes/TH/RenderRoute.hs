{-# LANGUAGE TemplateHaskell #-}
module Yesod.Routes.TH.RenderRoute
    ( -- ** RenderRoute
      mkRenderRouteInstance
    , mkRouteCons
    , mkRenderRouteClauses
    ) where

import Yesod.Routes.TH.Types
import Language.Haskell.TH.Syntax
import Data.Maybe (maybeToList)
import Control.Monad (replicateM)
import Data.Text (pack)
import Web.PathPieces (PathPiece (..), PathMultiPiece (..))
import Yesod.Routes.Class

-- | Generate the constructors of a route data type.
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

-- | Clauses for the 'renderRoute' method.
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

        pack' <- [|pack|]
        tsp <- [|toPathPiece|]
        let piecesSingle = mkPieces (AppE pack' . LitE . StringL) tsp (resourcePieces res) dyns

        piecesMulti <-
            case resourceMulti res of
                Nothing -> return $ ListE []
                Just{} -> do
                    tmp <- [|toPathMultiPiece|]
                    return $ tmp `AppE` VarE (last dyns)

        body <-
            case sub of
                [x] -> do
                    rr <- [|renderRoute|]
                    a <- newName "a"
                    b <- newName "b"

                    colon <- [|(:)|]
                    let cons y ys = InfixE (Just y) colon (Just ys)
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
    mkPieces _ _ ((Dynamic _) : _) [] = error "mkPieces 120"

-- | Generate the 'RenderRoute' instance.
--
-- This includes both the 'Route' associated type and the 'renderRoute' method.
-- This function uses both 'mkRouteCons' and 'mkRenderRouteClasses'.
mkRenderRouteInstance :: Type -> [Resource] -> Q Dec
mkRenderRouteInstance typ ress = do
    cls <- mkRenderRouteClauses ress
    return $ InstanceD [] (ConT ''RenderRoute `AppT` typ)
        [ DataInstD [] ''Route [typ] (mkRouteCons ress) clazzes
        , FunD (mkName "renderRoute") cls
        ]
  where
    clazzes = [''Show, ''Eq, ''Read]
