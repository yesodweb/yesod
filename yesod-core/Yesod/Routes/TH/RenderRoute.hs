{-# LANGUAGE TemplateHaskell, CPP #-}
module Yesod.Routes.TH.RenderRoute
    ( -- ** RenderRoute
      mkRenderRouteInstance
    , mkRenderRouteInstance'
    , mkRenderRouteInstance'
    , mkRouteCons
    , mkRenderRouteClauses
    ) where

import Yesod.Routes.TH.Types
#if MIN_VERSION_template_haskell(2,11,0)
import Language.Haskell.TH (conT)
#endif
import Language.Haskell.TH.Syntax
import Data.Bits (xor)
import Data.Maybe (maybeToList)
import Control.Monad (replicateM)
import Data.Text (pack)
import Web.PathPieces (PathPiece (..), PathMultiPiece (..))
import Yesod.Routes.Class
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
import Data.Monoid (mconcat)
#endif

-- | Generate the constructors of a route data type.
mkRouteCons :: [ResourceTree Type] -> Q ([Con], [Dec])
mkRouteCons rttypes =
    mconcat <$> mapM mkRouteCon rttypes
  where
    mkRouteCon (ResourceLeaf res) =
        return ([con], [])
      where
        con = NormalC (mkName $ resourceName res)
            $ map (\x -> (notStrict, x))
            $ concat [singles, multi, sub]
        singles = concatMap toSingle $ resourcePieces res
        toSingle Static{} = []
        toSingle (Dynamic typ) = [typ]

        multi = maybeToList $ resourceMulti res

        sub =
            case resourceDispatch res of
                Subsite { subsiteType = typ } -> [ConT ''Route `AppT` typ]
                _ -> []

    mkRouteCon (ResourceParent name _check pieces children) = do
        (cons, decs) <- mkRouteCons children
#if MIN_VERSION_template_haskell(2,12,0)
        dec <- DataD [] (mkName name) [] Nothing cons <$> fmap (pure . DerivClause Nothing) (mapM conT [''Show, ''Read, ''Eq])
#elif MIN_VERSION_template_haskell(2,11,0)
        dec <- DataD [] (mkName name) [] Nothing cons <$> mapM conT [''Show, ''Read, ''Eq]
#else
        let dec = DataD [] (mkName name) [] cons [''Show, ''Read, ''Eq]
#endif
        return ([con], dec : decs)
      where
        con = NormalC (mkName name)
            $ map (\x -> (notStrict, x))
            $ singles ++ [ConT $ mkName name]

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
                Subsite{} -> return <$> newName "sub"
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
    mkPieces _ _ (Dynamic _ : _) [] = error "mkPieces 120"

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
    (cons, decs) <- mkRouteCons ress
#if MIN_VERSION_template_haskell(2,12,0)
    did <- DataInstD [] ''Route [typ] Nothing cons <$> fmap (pure . DerivClause Nothing) (mapM conT (clazzes False))
#elif MIN_VERSION_template_haskell(2,11,0)
    did <- DataInstD [] ''Route [typ] Nothing cons <$> mapM conT (clazzes False)
#else
    let did = DataInstD [] ''Route [typ] cons (clazzes False)
#endif
    let sds = fmap (\t -> StandaloneDerivD cxt $ ConT t `AppT` ( ConT ''Route `AppT` typ)) (clazzes True)
    return $ instanceD cxt (ConT ''RenderRoute `AppT` typ)
        [ did
        , FunD (mkName "renderRoute") cls
        ]
        : sds ++ decs
  where
    clazzes standalone = if standalone `xor` null cxt then
          [''Show, ''Eq, ''Read]
        else
          []

#if MIN_VERSION_template_haskell(2,11,0)
notStrict :: Bang
notStrict = Bang NoSourceUnpackedness NoSourceStrictness
#else
notStrict :: Strict
notStrict = NotStrict
#endif

instanceD :: Cxt -> Type -> [Dec] -> Dec
#if MIN_VERSION_template_haskell(2,11,0)
instanceD = InstanceD Nothing
#else
instanceD = InstanceD
#endif
