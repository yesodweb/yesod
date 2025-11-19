{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Yesod.Routes.TH.RenderRoute
    ( -- ** RenderRoute
      mkRenderRouteInstanceOpts
    , mkRouteConsOpts
    , mkRenderRouteClauses
    , shouldCreateResources

    , RouteOpts
    , defaultOpts
    , setEqDerived
    , setShowDerived
    , setReadDerived
    , setFocusOnNestedRoute
    , roFocusOnNestedRoute
    , roNestedRouteFallthrough
    , setCreateResources
    , setParameterizedSubroute
    , setNestedRouteFallthrough
    ) where

import Yesod.Routes.TH.Types
import Language.Haskell.TH.Syntax
import Data.Maybe (maybeToList)
import Control.Monad (replicateM)
import Data.Text (pack)
import Web.PathPieces (PathPiece (..), PathMultiPiece (..))
import Yesod.Routes.Class
import Data.Foldable
import Yesod.Routes.TH.Internal

-- | General opts data type for generating yesod.
--
-- Contains options for customizing code generation for the router in
-- 'mkYesodData', including what type class instances will be derived for
-- the route datatype, whether to parameterize subroutes,
-- and whether or not to create the @resources :: [ResourceTree String]@ value.
-- Use the setting functions on `defaultOpts` to set specific fields.
--
-- @since 1.6.25.0
data RouteOpts = MkRouteOpts
    { roDerivedEq   :: Bool
    , roDerivedShow :: Bool
    , roDerivedRead :: Bool
    , roFocusOnNestedRoute :: Maybe String
    -- ^ If this option is set, then we will only generate datatypes for
    -- the nested subroute that matches this string.
    --
    -- @since 1.6.28.0
    , roCreateResources :: Bool
    , roParameterizedSubroute :: Bool
    , roNestedRouteFallthrough :: Bool
    -- ^ If 'True', then a nested route will fall through if it fails to
    -- match. If 'False', then a nested route will throw 'notFound' if it
    -- does not match.
    --
    -- Default: 'False'.
    --
    -- @since 1.6.28.0
    }

-- | Default options for generating routes.
--
-- Defaults to all instances derived, subroutes being unparameterized, and to
-- create the @resourcesSite :: [ResourceTree String]@ term.
--
-- @since 1.6.25.0
defaultOpts :: RouteOpts
defaultOpts = MkRouteOpts
    { roDerivedEq = True
    , roDerivedShow = True
    , roDerivedRead = True
    , roCreateResources = True
    , roParameterizedSubroute = False
    , roFocusOnNestedRoute = Nothing
    , roNestedRouteFallthrough = False
    }

-- | If you set this with @Just routeName@, then the code generation will
-- generate code for the @routeName@ to be imported in the main dispatch
-- class. This allows you to generate code in separate modules.
--
-- Example:
--
-- First, you would put your route definitions into their own file.
--
-- @
-- module App.Routes.Resources where
--
-- import Yesod.Core
--
-- appResources :: [ResourceTree String]
-- appResources = [parseRoutes|
--     /  HomeR GET
--
--     /nest NestR:
--         /     NestIndexR GET POST
--         /#Int NestShowR  GET POST
--
-- |]
-- @
--
-- We have defined a nested route called @NestR@ here. A nested route is
-- created with the @:@ after the route name.
--
-- Then, in a module for the route specifically, we can generate the route
-- datatype and instances for later hooking in to the main instance.
--
-- @
-- module App.Routes.NestR where
--
-- import App.Routes.Resources
-- import Yesod.Core
--
-- mkYesodOpts (setFocusOnNestedRoute (Just "NestR") defaultOptions) "App" appResources
-- @
--
-- If you only want to generate the datatypes, you can separate things
-- further using 'mkYesodDataOpts' and 'mkYesodDispatchOpts'.
--
-- Finally, import that type into your main yesod macro code.
--
-- @
-- module App where
--
-- import App.Routes.Resources (appResources)
-- import App.Routes.NestR (NestR(..))
--
-- mkYesod "App" appResources
-- @
--
-- The call to 'mkYesod' will delegate to the generated code for @NestR@
-- rather than regenerating it.
--
-- @since 1.6.28.0
setFocusOnNestedRoute :: Maybe String -> RouteOpts -> RouteOpts
setFocusOnNestedRoute mstr rdo = rdo { roFocusOnNestedRoute = mstr }

-- | When 'True', a nested route can fall through if it does not match.
--
-- @since 1.6.28.0
setNestedRouteFallthrough :: Bool -> RouteOpts -> RouteOpts
setNestedRouteFallthrough b rdo = rdo { roNestedRouteFallthrough = b }

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

-- | If True, we will correctly pass parameters for subroutes around.
--
-- @since 1.6.28.0
setParameterizedSubroute :: Bool -> RouteOpts -> RouteOpts
setParameterizedSubroute b rdo = rdo { roParameterizedSubroute = b }

-- |
--
-- @since 1.6.25.0
instanceNamesFromOpts :: RouteOpts -> [Name]
instanceNamesFromOpts MkRouteOpts {..} = prependIf roDerivedEq ''Eq $ prependIf roDerivedShow ''Show $ prependIf roDerivedRead ''Read []
    where prependIf b = if b then (:) else const id

-- | Nullify the list unless we are using parameterised subroutes.
nullifyWhenNoParam :: RouteOpts -> [a] -> [a]
nullifyWhenNoParam opts = if roParameterizedSubroute opts then id else const []

-- | Generate the constructors of a route data type, with custom opts.
--
-- @since 1.6.25.0
mkRouteConsOpts :: RouteOpts -> Cxt -> [(Type, Name)] -> [ResourceTree Type] -> Q ([Con], [Dec])
mkRouteConsOpts opts cxt (nullifyWhenNoParam opts -> tyargs) =
    mkRouteConsOpts'
  where
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

    subrouteDecTypeArgs = fmap (tyvarbndr . snd) tyargs

    (inlineDerives, mkStandaloneDerives) = getDerivesFor opts (nullifyWhenNoParam opts cxt)

    mkRouteConsOpts' :: [ResourceTree Type] -> Q ([Con], [Dec])
    mkRouteConsOpts' = foldMap mkRouteCon

    mkRouteCon :: ResourceTree Type -> Q ([Con], [Dec])
    mkRouteCon (ResourceLeaf res) =
        case roFocusOnNestedRoute opts of
            Nothing -> do
                pure ([con], [])
            Just _ -> do
                -- If we are focusing on a specific nested subroute, then
                -- a Leaf cannot possibly be what we want.
                pure ([], [])
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
        let newOpts =
                case roFocusOnNestedRoute opts of
                    Nothing ->
                        opts
                    Just target ->
                        if target == name
                            then setFocusOnNestedRoute Nothing opts
                            else opts
        (cons, decs) <- mkRouteConsOpts newOpts cxt tyargs children
        let childDataName = mkName name

        -- Generate the child datatype if it has not been generated
        -- already, but only if we are *not* focusing on some other
        -- datatype.
        mname' <- lookupTypeName name
        mdec <- case mname' of
            Just _ -> do
                -- datatype already exists, definitely don't generate it
                pure Nothing
            Nothing -> do
                case roFocusOnNestedRoute opts of
                    Just target | target /= name -> do
                        -- If we have a target, and this ain't it, don't
                        -- generate
                        pure Nothing
                    _ -> do
                        let childData = mkChildDataGen childDataName cons inlineDerives
                        let childDataType = foldl' AppT (ConT childDataName) (fst <$> tyargs)
                        childClauses <- mkRenderRouteClauses children
                        let childInstances =
                                InstanceD
                                    Nothing
                                    (cxt <> getRequiredContextFor opts (fst <$> tyargs))
                                    (ConT ''RenderRouteNested `AppT` consDataType)
                                    [ FunD 'renderRouteNested childClauses
                                    ]
                        pure $ Just (childData : [childInstances] ++ mkStandaloneDerives childDataType)

        return ([con], maybe id (<>) mdec decs)
      where
        con = NormalC dataName
            $ map (notStrict,)
            $ singles ++ [consDataType]

        singles = concatMap toSingle pieces
        toSingle Static{} = []
        toSingle (Dynamic typ) = [typ]

        mkChildDataGen :: Name -> [Con] -> [DerivClause] -> Dec
        mkChildDataGen childDataName cons conts =
            DataD [] childDataName subrouteDecTypeArgs Nothing cons conts
        dataName = mkName name
        consDataType = foldl' (\b a -> b `AppT` fst a) (ConT dataName) tyargs

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

        typeExists <- lookupTypeName name
        hasNestInstance <- case typeExists of
            Just _ ->
                isInstance ''RenderRouteNested [ConT (mkName name)]
            Nothing ->
                pure False


        childRender <- newName "childRender"
        let rr = VarE childRender
        childClauses <-
            if hasNestInstance
            then
                pure [Clause [] (NormalB (VarE 'renderRouteNested)) []]
            else
                mkRenderRouteClauses children

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
-- 'renderRoute' method.  This function uses both 'mkRouteConsOpts' and
-- 'mkRenderRouteClauses'.
--
-- @since 1.6.25.0
mkRenderRouteInstanceOpts
    :: RouteOpts
    -> Cxt
    -- ^ The context passed around for the instances
    -> [(Type, Name)]
    -- ^ Type arguments
    -> Type
    -- ^ The type of the foundation resource
    -> [ResourceTree Type]
    -- ^ The actual tree of routes to generate code for
    -> Q [Dec]
mkRenderRouteInstanceOpts opts cxt tyargs typ ress = do
    cls <- mkRenderRouteClauses ress
    (cons, decs) <- mkRouteConsOpts opts cxt tyargs ress
    let did = DataInstD []
#if MIN_VERSION_template_haskell(2,15,0)
            Nothing routeDataName
#else
            ''Route [typ]
#endif
            Nothing cons inlineDerives
    case roFocusOnNestedRoute opts of
        Nothing -> do
            return $ instanceD cxt (ConT ''RenderRoute `AppT` typ)
                [ did
                , FunD (mkName "renderRoute") cls
                ]
                : mkStandaloneDerives routeDataName ++ decs
        Just _ -> do
            -- If we're generating routes for a subtarget, then we won't
            -- generate the top-level `RenderRoute`. Instead, we'll want to
            -- only generate the `decs` that are returned, plus the child
            -- class declaration, eventually.
            pure decs
  where
    routeDataName = ConT ''Route `AppT` typ
    (inlineDerives, mkStandaloneDerives) = getDerivesFor opts cxt

-- | Get the simple derivation clauses and the standalone derivation clauses
-- for a given type and context.
--
-- If there are any additional classes needed for context, we just produce standalone
-- clauses. Else, we produce basic deriving clauses for a declaration.
getDerivesFor :: RouteOpts -> Cxt -> ([DerivClause], Type ->  [Dec])
getDerivesFor opts cxt
    | null cxt =
        ( [DerivClause Nothing clazzes']
        , const []
        )
    | otherwise =
        ( []
        , \typ -> fmap (StandaloneDerivD Nothing cxt . (`AppT` typ)) clazzes'
        )
  where
    clazzes' = ConT <$> instanceNamesFromOpts opts

getRequiredContextFor :: RouteOpts -> Cxt -> Cxt
getRequiredContextFor opts cxt
    | null cxt =
        []
    | otherwise = do
        typ <- cxt
        clazz <- clazzes'
        pure (clazz `AppT` typ)
  where
    clazzes' = ConT <$> instanceNamesFromOpts opts

notStrict :: Bang
notStrict = Bang NoSourceUnpackedness NoSourceStrictness

instanceD :: Cxt -> Type -> [Dec] -> Dec
instanceD = InstanceD Nothing
