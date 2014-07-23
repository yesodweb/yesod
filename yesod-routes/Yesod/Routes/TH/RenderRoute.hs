{-# LANGUAGE TemplateHaskell #-}
module Yesod.Routes.TH.RenderRoute
    ( -- ** RenderRoute
      mkRenderRouteInstance
    , mkRenderRouteInstance'
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
import Data.Monoid (mconcat)

-- | Generate the constructors of a route data type.
mkRouteCons :: [ResourceTree Type] -> ([Con], [Dec])
mkRouteCons =
    mconcat . map mkRouteCon
  where
    mkRouteCon (ResourceLeaf res) =
        ([con], [])
      where
        con = NormalC (mkName $ resourceName res)
            $ map (\x -> (NotStrict, x))
            $ concat [singles, multi, sub]
        singles = concatMap toSingle $ resourcePieces res
        toSingle Static{} = []
        toSingle (Dynamic typ) = [typ]

        multi = maybeToList $ resourceMulti res

        sub =
            case resourceDispatch res of
                Subsite { subsiteType = typ } -> [ConT ''Route `AppT` typ]
                _ -> []
    mkRouteCon (ResourceParent name _check pieces children) =
        ([con], dec : decs)
      where
        (cons, decs) = mkRouteCons children
        con = NormalC (mkName name)
            $ map (\x -> (NotStrict, x))
            $ concat [singles, [ConT $ mkName name]]
        dec = DataD [] (mkName name) [] cons [''Show, ''Read, ''Eq]

        singles = concatMap toSingle pieces
        toSingle Static{} = []
        toSingle (Dynamic typ) = [typ]

-- | Clauses for the 'renderRoute' method.
mkRenderRouteClauses :: [ResourceTree Type] -> Q [Clause]
mkRenderRouteClauses =
    mapM go
  where
    isDynamic Dynamic{} = True
    isDynamic _ = False

    go (ResourceParent name _check pieces children) = do
        let cnt = length $ filter isDynamic pieces
        dyns <- replicateM cnt $ newName "dyn"
        child <- newName "child"
        let pat = ConP (mkName name) $ map VarP $ dyns ++ [child]

        pack' <- [|pack|]
        tsp <- [|toPathPiece|]
        let piecesSingle = mkPieces (AppE pack' . LitE . StringL) tsp pieces dyns

        childRender <- newName "childRender"
        let rr = VarE childRender
        childClauses <- mkRenderRouteClauses children

        a <- newName "a"
        b <- newName "b"

        colon <- [|(:)|]
        let cons y ys = InfixE (Just y) colon (Just ys)
        let pieces' = foldr cons (VarE a) piecesSingle

        let body = LamE [TupP [VarP a, VarP b]] (TupE [pieces', VarE b]) `AppE` (rr `AppE` VarE child)

        return $ Clause [pat] (NormalB body) [FunD childRender childClauses]

    go (ResourceLeaf res) = do
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
-- This includes both the 'Route' associated type and the
-- 'renderRoute' method.  This function uses both 'mkRouteCons' and
-- 'mkRenderRouteClasses'.
mkRenderRouteInstance :: Type -> [ResourceTree Type] -> Q [Dec]
mkRenderRouteInstance = mkRenderRouteInstance' []

-- | A more general version of 'mkRenderRouteInstance' which takes an
-- additional context.

mkRenderRouteInstance' :: Cxt -> Type -> [ResourceTree Type] -> Q [Dec]
mkRenderRouteInstance' cxt typ ress = do
    cls <- mkRenderRouteClauses ress
    let (cons, decs) = mkRouteCons ress
    return $ InstanceD cxt (ConT ''RenderRoute `AppT` typ)
        [ DataInstD [] ''Route [typ] cons clazzes
        , FunD (mkName "renderRoute") cls
        ] : decs
  where
    clazzes = [''Show, ''Eq, ''Read]
