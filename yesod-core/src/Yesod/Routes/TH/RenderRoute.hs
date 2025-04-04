{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}

module Yesod.Routes.TH.RenderRoute
    ( -- ** RenderRoute
      mkRenderRouteInstance
    , mkRenderRouteInstanceOpts
    , mkRouteCons
    , mkRouteConsOpts
    , mkRenderRouteClauses
    , shouldCreateResources

    , RouteOpts
    , defaultOpts
    , setEqDerived
    , setShowDerived
    , setReadDerived
    , setCreateResources
    ) where

import Yesod.Routes.TH.Types
import Language.Haskell.TH (conT)
import Language.Haskell.TH.Syntax
import Data.Maybe (maybeToList)
import Control.Monad (replicateM)
import Data.Text (pack)
import Web.PathPieces (PathPiece (..), PathMultiPiece (..))
import Yesod.Routes.Class
import Data.Foldable

-- | General opts data type for generating yesod.
--
-- Contains options for customizing code generation for the router in
-- 'mkYesodData', including what type class instances will be derived for
-- the route datatype and whether or not to create the @resources ::
-- [ResourceTree String]@ value. Use the setting functions on `defaultOpts`
-- to set specific fields.
--
-- @since 1.6.25.0
data RouteOpts = MkRouteOpts
    { roDerivedEq   :: Bool
    , roDerivedShow :: Bool
    , roDerivedRead :: Bool
    , roCreateResources :: Bool
    }

-- | Default options for generating routes.
--
-- Defaults to all instances derived and to create the @resourcesSite ::
-- [ResourceTree String]@ term.
--
-- @since 1.6.25.0
defaultOpts :: RouteOpts
defaultOpts = MkRouteOpts
    { roDerivedEq = True
    , roDerivedShow = True
    , roDerivedRead = True
    , roCreateResources = True
    }

-- |
--
-- @since 1.6.25.0
setEqDerived :: Bool -> RouteOpts -> RouteOpts
setEqDerived b rdo = rdo { roDerivedEq = b }

-- |
--
-- @since 1.6.25.0
setShowDerived :: Bool -> RouteOpts -> RouteOpts
setShowDerived b rdo = rdo { roDerivedShow = b }

-- |
--
-- @since 1.6.25.0
setReadDerived :: Bool -> RouteOpts -> RouteOpts
setReadDerived b rdo = rdo { roDerivedRead = b }

-- | Determine whether or not to generate the @resourcesApp@ value.
--
-- Disabling this can be useful if you are creating the @routes ::
-- [ResourceTree String]@ elsewhere in your module, and referring to it
-- here. The @resourcesApp@ can become very large in large applications,
-- and duplicating it can result in signifiacntly higher compile times.
--
-- @since 1.6.28.0
setCreateResources :: Bool -> RouteOpts -> RouteOpts
setCreateResources b rdo = rdo { roCreateResources = b }

-- | Returns whether or not we should create the @resourcesSite ::
-- [ResourceTree String]@ value during code generation.
--
-- @since 1.6.28.0
shouldCreateResources :: RouteOpts -> Bool
shouldCreateResources = roCreateResources

-- |
--
-- @since 1.6.25.0
instanceNamesFromOpts :: RouteOpts -> [Name]
instanceNamesFromOpts MkRouteOpts {..} = prependIf roDerivedEq ''Eq $ prependIf roDerivedShow ''Show $ prependIf roDerivedRead ''Read []
    where prependIf b = if b then (:) else const id

-- |
--
-- @since 1.6.28.0

-- | Generate the constructors of a route data type.
mkRouteCons :: [ResourceTree Type] -> Q ([Con], [Dec])
mkRouteCons = mkRouteConsOpts defaultOpts [] []

-- | Generate the constructors of a route data type, with custom opts.
--
-- @since 1.6.25.0
mkRouteConsOpts :: RouteOpts -> Cxt -> [(Type, Name)] -> [ResourceTree Type] -> Q ([Con], [Dec])
mkRouteConsOpts opts cxt tyargs rttypes =
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
        (cons, decs) <- mkRouteConsOpts opts cxt tyargs children
        dec <- DataD [] dataName (fmap (tyvarbndr . snd) tyargs) Nothing cons <$> inlineDerives
        return ([con], dec : decs ++ sds)
      where
        con = NormalC dataName
            $ map (notStrict,)
            $ singles ++ [consDataType]

        -- th-abstraction does cover this but the version it was introduced in
        -- isn't always available
        tyvarbndr =
#if MIN_VERSION_template_haskell(2,21,0)
            (`PlainTV` BndrReq) :: Name -> TyVarBndr BndrVis
#elif MIN_VERSION_template_haskell(2,17,0)
            (`PlainTV` ()) :: Name -> TyVarBndr ()
#else
            PlainTV :: Name -> TyVarBndr
#endif

        singles = concatMap toSingle pieces
        toSingle Static{} = []
        toSingle (Dynamic typ) = [typ]

        dataName = mkName name
        consDataType = foldl' (\b a -> b `AppT` fst a) (ConT dataName) tyargs

        (inlineDerives, sds) = getDerivesFor opts cxt consDataType

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
mkRenderRouteInstance cxt = mkRenderRouteInstanceOpts defaultOpts cxt []

-- | Generate the 'RenderRoute' instance.
--
-- This includes both the 'Route' associated type and the
-- 'renderRoute' method.  This function uses both 'mkRouteCons' and
-- 'mkRenderRouteClasses'.
--
-- @since 1.6.25.0
mkRenderRouteInstanceOpts :: RouteOpts -> Cxt -> [(Type, Name)] -> Type -> [ResourceTree Type] -> Q [Dec]
mkRenderRouteInstanceOpts opts cxt tyargs typ ress = do
    cls <- mkRenderRouteClauses ress
    (cons, decs) <- mkRouteConsOpts opts cxt tyargs ress
#if MIN_VERSION_template_haskell(2,15,0)
    did <- DataInstD [] Nothing (AppT (ConT ''Route) typ) Nothing cons <$> inlineDerives
#else
    did <- DataInstD [] ''Route [typ] Nothing cons <$> inlineDerives
#endif
    return $ instanceD cxt (ConT ''RenderRoute `AppT` typ)
        [ did
        , FunD (mkName "renderRoute") cls
        ]
        : sds ++ decs
  where
    (inlineDerives, sds) = getDerivesFor opts cxt ( ConT ''Route `AppT` typ)

#if MIN_VERSION_template_haskell(2,12,0)
getDerivesFor :: RouteOpts -> Cxt -> Type -> (Q [DerivClause], [Dec])
#else
getDerivesFor :: RouteOpts -> Cxt -> Type -> (Q Cxt, [Dec])
#endif
getDerivesFor opts cxt typ
    | null cxt = (
#if MIN_VERSION_template_haskell(2,12,0)
            fmap (pure . DerivClause Nothing)
#endif
            aClause, [])
    | otherwise = (pure [], fmap (\t -> StandaloneDerivD
#if MIN_VERSION_template_haskell(2,12,0)
            Nothing
#endif
            cxt $ ConT t `AppT` typ) clazzes')
    where
    aClause = mapM conT clazzes'
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
