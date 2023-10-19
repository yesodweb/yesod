{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Yesod.Routes.TH.RenderRoute
    ( -- ** RenderRoute
      mkRenderRouteInstance
    , mkRenderRouteInstanceOpts
    , mkRouteCons
    , mkRouteConsOpts
    , mkRenderRouteClauses

    , RouteOpts
    , defaultOpts
    , setEqDerived
    , setShowDerived
    , setReadDerived
    ) where

import Yesod.Routes.TH.Types
import Language.Haskell.TH (conT)
import Language.Haskell.TH.Syntax
import Data.Bits (xor)
import Data.Maybe (maybeToList)
import Control.Monad (replicateM)
import Data.Text (pack)
import Web.PathPieces (PathPiece (..), PathMultiPiece (..))
import Yesod.Routes.Class

-- | General opts data type for generating yesod.
--
-- Contains options for what instances are derived for the route. Use the setting
-- functions on `defaultOpts` to set specific fields.
data RouteOpts = MkRouteOpts
    { roDerivedEq   :: Bool
    , roDerivedShow :: Bool
    , roDerivedRead :: Bool
    }

-- | Default options for generating routes.
--
-- Defaults to all instances derived.
defaultOpts :: RouteOpts
defaultOpts = MkRouteOpts True True True

setEqDerived :: Bool -> RouteOpts -> RouteOpts
setEqDerived b rdo = rdo { roDerivedEq = b }

setShowDerived :: Bool -> RouteOpts -> RouteOpts
setShowDerived b rdo = rdo { roDerivedShow = b }

setReadDerived :: Bool -> RouteOpts -> RouteOpts
setReadDerived b rdo = rdo { roDerivedRead = b }

instanceNamesFromOpts :: RouteOpts -> [Name]
instanceNamesFromOpts (MkRouteOpts eq shw rd) = prependIf eq ''Eq $ prependIf shw ''Show $ prependIf rd ''Read []
    where prependIf b = if b then (:) else const id

-- | Generate the constructors of a route data type.
mkRouteCons :: [ResourceTree Type] -> Q ([Con], [Dec])
mkRouteCons = mkRouteConsOpts defaultOpts

-- | Generate the constructors of a route data type, with custom opts.
mkRouteConsOpts :: RouteOpts -> [ResourceTree Type] -> Q ([Con], [Dec])
mkRouteConsOpts opts rttypes =
    mconcat <$> mapM mkRouteCon rttypes
  where
    mkRouteCon (ResourceLeaf res) =
        return ([con], [])
      where
        con = NormalC (mkName $ resourceName res)
            $ map (notStrict,)
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
        (cons, decs) <- mkRouteConsOpts opts children
        let conts = mapM conT $ instanceNamesFromOpts opts
#if MIN_VERSION_template_haskell(2,12,0)
        dec <- DataD [] (mkName name) [] Nothing cons <$> fmap (pure . DerivClause Nothing) conts
#else
        dec <- DataD [] (mkName name) [] Nothing cons <$> conts
#endif
        return ([con], dec : decs)
      where
        con = NormalC (mkName name)
            $ map (notStrict,)
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
        let pat = conPCompat (mkName name) $ map VarP $ dyns ++ [child]

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

        let body = LamE [TupP [VarP a, VarP b]] (TupE
#if MIN_VERSION_template_haskell(2,16,0)
                                                  $ map Just
#endif
                                                  [pieces', VarE b]
                                                ) `AppE` (rr `AppE` VarE child)

        return $ Clause [pat] (NormalB body) [FunD childRender childClauses]

    go (ResourceLeaf res) = do
        let cnt = length (filter isDynamic $ resourcePieces res) + maybe 0 (const 1) (resourceMulti res)
        dyns <- replicateM cnt $ newName "dyn"
        sub <-
            case resourceDispatch res of
                Subsite{} -> return <$> newName "sub"
                _ -> return []
        let pat = conPCompat (mkName $ resourceName res) $ map VarP $ dyns ++ sub

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

                    return $ LamE [TupP [VarP a, VarP b]] (TupE
#if MIN_VERSION_template_haskell(2,16,0)
                                                            $ map Just
#endif
                                                            [pieces, VarE b]
                                                          ) `AppE` (rr `AppE` VarE x)
                _ -> do
                    colon <- [|(:)|]
                    let cons a b = InfixE (Just a) colon (Just b)
                    return $ TupE
#if MIN_VERSION_template_haskell(2,16,0)
                      $ map Just
#endif
                      [foldr cons piecesMulti piecesSingle, ListE []]

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
mkRenderRouteInstance :: Cxt -> Type -> [ResourceTree Type] -> Q [Dec]
mkRenderRouteInstance = mkRenderRouteInstanceOpts defaultOpts

-- | Generate the 'RenderRoute' instance.
--
-- This includes both the 'Route' associated type and the
-- 'renderRoute' method.  This function uses both 'mkRouteCons' and
-- 'mkRenderRouteClasses'.
mkRenderRouteInstanceOpts :: RouteOpts -> Cxt -> Type -> [ResourceTree Type] -> Q [Dec]
mkRenderRouteInstanceOpts opts cxt typ ress = do
    cls <- mkRenderRouteClauses ress
    (cons, decs) <- mkRouteConsOpts opts ress
#if MIN_VERSION_template_haskell(2,15,0)
    did <- DataInstD [] Nothing (AppT (ConT ''Route) typ) Nothing cons <$> fmap (pure . DerivClause Nothing) (mapM conT (clazzes False))
    let sds = fmap (\t -> StandaloneDerivD Nothing cxt $ ConT t `AppT` ( ConT ''Route `AppT` typ)) (clazzes True)
#elif MIN_VERSION_template_haskell(2,12,0)
    did <- DataInstD [] ''Route [typ] Nothing cons <$> fmap (pure . DerivClause Nothing) (mapM conT (clazzes False))
    let sds = fmap (\t -> StandaloneDerivD Nothing cxt $ ConT t `AppT` ( ConT ''Route `AppT` typ)) (clazzes True)
#else
    did <- DataInstD [] ''Route [typ] Nothing cons <$> mapM conT (clazzes False)
    let sds = fmap (\t -> StandaloneDerivD cxt $ ConT t `AppT` ( ConT ''Route `AppT` typ)) (clazzes True)
#endif
    return $ instanceD cxt (ConT ''RenderRoute `AppT` typ)
        [ did
        , FunD (mkName "renderRoute") cls
        ]
        : sds ++ decs
  where
    clazzes standalone = if standalone `xor` null cxt then
          clazzes'
        else
          []
    clazzes' = instanceNamesFromOpts opts

notStrict :: Bang
notStrict = Bang NoSourceUnpackedness NoSourceStrictness

instanceD :: Cxt -> Type -> [Dec] -> Dec
instanceD = InstanceD Nothing

conPCompat :: Name -> [Pat] -> Pat
conPCompat n pats = ConP n
#if MIN_VERSION_template_haskell(2,18,0)
                         []
#endif
                         pats
