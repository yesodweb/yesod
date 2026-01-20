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
    , mkRenderRouteNestedClauses
    , shouldCreateResources

    , RouteOpts(MkRouteOpts)
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
    , nullifyWhenNoParam
    ) where

import Data.Maybe
import Yesod.Routes.TH.Types
import Language.Haskell.TH.Syntax
import Control.Monad
import Data.Text (pack)
import Web.PathPieces (PathPiece (..), PathMultiPiece (..))
import Yesod.Routes.Class
import Data.Foldable
import Yesod.Routes.TH.Internal
import Yesod.Core.Class.Dispatch
import Data.Char
import Yesod.Core.Class.Dispatch.ToParentRoute

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

-- | Generate the constructors of a route data type, with custom
-- 'RouteOpts'.
--
-- The first element in the return is the list of constructors for the
-- @Route site@ datatype. The second element is the list of 'Dec' to
-- declare nested datatypes or class instances of the 'RenderRouteNested'
-- for those instances.
--
-- @since 1.6.25.0
mkRouteConsOpts :: RouteOpts -> Cxt -> [(Type, Name)] -> Type -> [ResourceTree Type] -> Q ([Con], [Dec])
mkRouteConsOpts opts cxt (nullifyWhenNoParam opts -> tyargs) master resourceTrees = do
    (prePieces, focusedTrees) <-
        case roFocusOnNestedRoute opts of
            Nothing ->
                pure ([], resourceTrees)
            Just target -> do
                let mnestedRoute = findNestedRoute target resourceTrees
                case mnestedRoute of
                    Nothing ->
                        fail $ "Failed to find target '" <> target <> "' in routes"
                    Just r ->
                        pure r
    mkRouteConsOpts' prePieces focusedTrees
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

    mkRouteConsOpts' :: [Piece Type] -> [ResourceTree Type] -> Q ([Con], [Dec])
    mkRouteConsOpts' prePieces = foldMap (mkRouteCon prePieces)

    mkRouteCon :: [Piece Type] -> ResourceTree Type -> Q ([Con], [Dec])
    mkRouteCon _ (ResourceLeaf res) =
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

    mkRouteCon prePieces (ResourceParent name _check pieces children) = do
        -- Accumulate pieces for children: combine parent pieces with this route's pieces
        let accumulatedPieces = prePieces <> pieces
        (cons, decs) <- mkRouteConsOpts' accumulatedPieces children
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
                        -- either we have `Just target | target == name`
                        -- (aka we are matching a target, and this is it)
                        -- or `Nothing` (aka we are either not doing
                        -- a target, or we have already found our target)
                        let childData = mkChildDataGen childDataName cons inlineDerives
                        let childDataType = foldl' AppT (ConT childDataName) (fst <$> tyargs)
                        let piecesAndNames =
                                map
                                    (\p -> case p of
                                        Static str -> Left str
                                        Dynamic a -> Right a)
                                    (prePieces <> pieces)
                            preDyns =
                                mapMaybe (either (const Nothing) Just) piecesAndNames

                        parentDynT <-
                            case preDyns of
                                [] -> [t| () |]
                                [t] -> pure t
                                ts ->
                                    pure $ foldl' AppT (TupleT (length ts)) ts
                        parentNames <- forM piecesAndNames $ \epiece'name -> do
                            case epiece'name of
                                Left piece ->
                                    pure (Left piece)
                                Right t -> do
                                    let tyName =
                                            filter isAlphaNum
                                            $ show t
                                        lowerFirst xs =
                                            case xs of
                                                (a : as) -> toLower a : as
                                                [] -> error "empty name????"
                                    Right <$> newName (lowerFirst tyName)
                        parentSiteT <- [t| ParentSite $(pure consDataType) |]
                        parentDynSig <- [t| ParentArgs $(pure consDataType) |]
                        (childClauses, _childNames) <- mkRenderRouteNestedClauses parentNames children
                        let childInstances =
                                InstanceD
                                    Nothing
                                    cxt
                                    (ConT ''RenderRouteNested `AppT` consDataType)
                                    [ TySynInstD $ TySynEqn Nothing parentSiteT master
                                    , TySynInstD $ TySynEqn Nothing parentDynSig parentDynT
                                    , FunD 'renderRouteNested childClauses
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

-- | Clauses for the 'renderRoute' method. This should be called from the
-- instance derivation for 'RenderRoute'. Also returns a list of 'String'
-- corresponding to types that don't yet have 'RenderRouteNested'
-- instances.
mkRenderRouteClauses :: [ResourceTree Type] -> Q ([Clause], [String])
mkRenderRouteClauses =
    fmap mconcat . mapM go
  where
    isDynamic Dynamic{} = True
    isDynamic _ = False

    go (ResourceParent name _check pieces _children) = do
        let cnt = length $ filter isDynamic pieces
        dyns <- replicateM cnt $ newName "dyn"
        child <- newName "child"
        let pat = conPCompat (mkName name) $ map VarP $ dyns ++ [child]

        typeExists <- lookupTypeName name
        hasNestInstance <- case typeExists of
            Just _ ->
                isInstance ''RenderRouteNested [ConT (mkName name)]
            Nothing ->
                pure False

        let parentArgs =
                case dyns of
                    [a] -> VarE a
                    _ -> mkTupE (map VarE dyns)
        let renderRouteNestedCall =
                VarE 'renderRouteNested `AppE` parentArgs `AppE` VarE child
            childNames =
                if hasNestInstance
                then []
                else [name]

        pure
            ( [Clause [pat] (NormalB renderRouteNestedCall) []]
            , childNames
            )

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

        return ( [Clause [pat] (NormalB body) []], [])

    mkPieces _ _ [] _ = []
    mkPieces toText tsp (Static s:ps) dyns = toText s : mkPieces toText tsp ps dyns
    mkPieces toText tsp (Dynamic{}:ps) (d:dyns) = tsp `AppE` VarE d : mkPieces toText tsp ps dyns
    mkPieces _ _ (Dynamic _ : _) [] = error "mkPieces 120"

-- | Like 'mkRenderRouteClauses', but instead generates clauses for the
-- definition of 'renderRouteNested'.
mkRenderRouteNestedClauses
    :: [Either String Name]
    -- ^ Either the static path piece or the names of the tuple of the parent arg.
    --
    -- These are both necessary to re-synthesize the total route.
    -> [ResourceTree Type]
    -> Q ([Clause], [String])
mkRenderRouteNestedClauses parentArgsNames resources = do
    fmap mconcat . mapM go $ resources
  where
    isDynamic Dynamic{} = True
    isDynamic _ = False

    go (ResourceParent name _check pieces _children) = do
        let cnt = length $ filter isDynamic pieces
        dyns <- replicateM cnt $ newName "dyn"
        child <- newName "child"
        let pat = conPCompat (mkName name) $ map VarP $ dyns ++ [child]

        typeExists <- lookupTypeName name
        hasNestInstance <- case typeExists of
            Just _ ->
                isInstance ''RenderRouteNested [ConT (mkName name)]
            Nothing ->
                pure False

        -- Extract parent dynamic variables from parentArgsNames
        let parentDyns = mapMaybe (either (const Nothing) Just) parentArgsNames

        -- Accumulate parent args: combine parent dynamic vars with this route's dynamic vars
        let allDyns = parentDyns ++ dyns
            accumulatedParentArgsExp =
                case allDyns of
                    [] -> ConE '()
                    [a] -> VarE a
                    _ -> mkTupE (map VarE allDyns)

            -- Pattern for extracting parent args from the function parameter
            parentArgsPat =
                case map VarP parentDyns of
                    [] -> WildP
                    [a] -> a
                    pats -> TupP pats

        -- The body should call renderRouteNested with accumulated parent args
        let renderRouteNestedCall =
                VarE 'renderRouteNested `AppE` accumulatedParentArgsExp `AppE` VarE child

            childNames =
                if hasNestInstance
                then []
                else [name]

        pure
            ( [Clause [parentArgsPat, pat] (NormalB renderRouteNestedCall) []]
            , childNames
            )

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

        -- Build the FULL path including parent pieces
        -- parentArgsNames contains Either String Name for each piece in the parent path
        let parentDyns = mapMaybe (either (const Nothing) Just) parentArgsNames
            parentArgsPat =
                case map VarP parentDyns of
                    [] -> WildP
                    [a] -> a
                    pats -> TupP pats

        -- Build parent path pieces from parentArgsNames
        let parentPieces = mkParentPieces pack' tsp parentArgsNames parentDyns
            -- Build this resource's own pieces
            thisResourcePieces = mkPieces (AppE pack' . LitE . StringL) tsp (resourcePieces res) dyns

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
                    -- Combine parent pieces with this resource's pieces
                    let allPieces = parentPieces ++ thisResourcePieces
                    let pieces = foldr cons (VarE a) allPieces

                    return $ LamE [TupP [VarP a, VarP b]] (TupE
#if MIN_VERSION_template_haskell(2,16,0)
                                                            $ map Just
#endif
                                                            [pieces, VarE b]
                                                          ) `AppE` (rr `AppE` VarE x)
                _ -> do
                    colon <- [|(:)|]
                    let cons a b = InfixE (Just a) colon (Just b)
                    -- Combine parent pieces with this resource's pieces
                    let allPieces = parentPieces ++ thisResourcePieces
                    return $ TupE
#if MIN_VERSION_template_haskell(2,16,0)
                      $ map Just
#endif
                      [foldr cons piecesMulti allPieces, ListE []]

        return ( [Clause [parentArgsPat, pat] (NormalB body) []], [])

    mkPieces _ _ [] _ = []
    mkPieces toText tsp (Static s:ps) dyns = toText s : mkPieces toText tsp ps dyns
    mkPieces toText tsp (Dynamic{}:ps) (d:dyns) = tsp `AppE` VarE d : mkPieces toText tsp ps dyns
    mkPieces _ _ (Dynamic _ : _) [] = error "mkPieces 120"

    -- Build path pieces from parentArgsNames (which contains both static and dynamic pieces)
    mkParentPieces :: Exp -> Exp -> [Either String Name] -> [Name] -> [Exp]
    mkParentPieces pack' tsp parentArgsNames' parentDyns = goParentPieces parentArgsNames' parentDyns
      where
        goParentPieces [] _ = []
        goParentPieces (Left staticStr : rest) dyns =
            (pack' `AppE` LitE (StringL staticStr)) : goParentPieces rest dyns
        goParentPieces (Right _ : rest) (dyn : dyns) =
            (tsp `AppE` VarE dyn) : goParentPieces rest dyns
        goParentPieces (Right _ : _) [] = error "mkParentPieces: dynamic piece without corresponding variable"

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
    case roFocusOnNestedRoute opts of
        Nothing -> do
            (cls, _names) <- mkRenderRouteClauses ress
            (cons, decs) <- mkRouteConsOpts opts cxt tyargs typ ress
            let did = DataInstD []
#if MIN_VERSION_template_haskell(2,15,0)
                    Nothing routeDataName
#else
                    ''Route [typ]
#endif
                    Nothing cons inlineDerives
            parentRouteInstancesDecs <- mkToParentRouteInstances opts cxt tyargs ress
            pure $ mconcat
                [ pure $ instanceD cxt (ConT ''RenderRoute `AppT` typ)
                    [ did
                    , FunD (mkName "renderRoute") cls
                    ]
                , mkStandaloneDerives routeDataName
                , decs
                , parentRouteInstancesDecs
                ]
        Just target ->
            case findNestedRoute target ress of
                Nothing ->
                    fail $ "Failed to find '" <> target <> "' in routes"
                Just (prepieces, ress') ->
                    mkRenderRouteNestedInstanceOpts opts cxt tyargs typ prepieces target ress'
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

-- | For each datatype, generate an instance of 'ToParentRoute' for the
-- datatype. Instances should mostly look like this:
--
-- > instance ToParentRoute FooR where
-- >     toParentRoute (a0, a1) = FooR a0 a1
mkToParentRouteInstances :: RouteOpts -> Cxt -> [(Type, Name)] -> [ResourceTree  Type] -> Q [Dec]
mkToParentRouteInstances routeOpts cxt (nullifyWhenNoParam routeOpts -> tyargs) ress = do
    mconcat <$> mapM (go ([], [])) ress
  where
    go _ (ResourceLeaf _) =
        pure []
    go (accPieces, parentConstructors) (ResourceParent name _check pieces children) = do
        -- Extract dynamic types from accumulated parent pieces
        let accDynTypes = [t | Dynamic t <- accPieces]
        accDynVars <- mapM (\_ -> newName "parent") accDynTypes

        -- Extract dynamic types from this route's pieces
        let piecesDynTypes = [t | Dynamic t <- pieces]
        piecesDynVars <- mapM (\_ -> newName "piece") piecesDynTypes

        -- Child route variable
        child <- newName "child"

        -- ParentArgs includes BOTH acc and pieces dynamic vars
        let allParentDynVars = accDynVars ++ piecesDynVars
            parentArgsPat = case map VarP allParentDynVars of
                [] -> WildP
                [a] -> a
                pats -> TupP pats

        -- Build the full route by applying all parent constructors
        -- We need to partition allParentDynVars according to each constructor's piece count
        let applyConToParentArgs = buildRouteExpr parentConstructors (mkName name) allParentDynVars (VarE child)

        let toParentRouteD =
                FunD 'toParentRoute
                    [ Clause
                        [parentArgsPat, VarP child]
                        (NormalB applyConToParentArgs)
                        []
                    ]

        let thisInstance =
                instanceD cxt (ConT ''ToParentRoute `AppT` applyTypeVariables name) [toParentRouteD]

        -- Accumulate pieces and constructor info for children
        let thisPieceCount = length piecesDynTypes
            acc' =
                ( accPieces <> pieces
                , parentConstructors ++ [(mkName name, thisPieceCount)]
                )

        childrenInstances <- mconcat <$> mapM (go acc') children
        pure $ thisInstance : childrenInstances

    applyTypeVariables name =
        foldl' (\t x -> t `AppT` fst x) (ConT (mkName name)) tyargs

    -- Build the route expression by applying constructors from outermost to innermost
    buildRouteExpr :: [(Name, Int)] -> Name -> [Name] -> Exp -> Exp
    buildRouteExpr parentCtors thisCtor allDynVars childExpr =
        let -- All constructors including this one
            thisPieceCount = length allDynVars - sum (map snd parentCtors)
            allCtors = parentCtors ++ [(thisCtor, thisPieceCount)]

            -- Partition dynamic vars for each constructor
            partitionedVars = go' allDynVars allCtors
              where
                go' _ [] = []
                go' vars ((_, count) : rest) =
                    let (varsForThis, remaining) = splitAt count vars
                    in varsForThis : go' remaining rest

            -- Build expression from inside out using foldr
            finalExpr = foldr
                (\(ctorName, varsForThis) innerExpr ->
                    foldl' AppE (ConE ctorName) (map VarE varsForThis ++ [innerExpr]))
                childExpr
                (zip (map fst allCtors) partitionedVars)
        in finalExpr

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

mkRenderRouteNestedInstanceOpts
    :: RouteOpts
    -> Cxt
    -> [(Type, Name)]
    -> Type
    -> [Piece Type]
    -> String
    -> [ResourceTree Type]
    -> Q [Dec]
mkRenderRouteNestedInstanceOpts routeOpts cxt tyargs typ prepieces target ress = do
    -- Generate constructors for all children
    (cons, _childDecs) <- mkRouteConsOpts' prepieces ress

    -- Create the datatype name
    let targetName = mkName target
        targetDataType = foldl' (\b a -> b `AppT` fst a) (ConT targetName) tyargs

    -- Set up type arguments for the datatype declaration
    let tyvarbndr =
#if MIN_VERSION_template_haskell(2,21,0)
            (`PlainTV` BndrReq) :: Name -> TyVarBndr BndrVis
#elif MIN_VERSION_template_haskell(2,17,0)
            (`PlainTV` ()) :: Name -> TyVarBndr ()
#else
            PlainTV :: Name -> TyVarBndr
#endif
        subrouteDecTypeArgs = fmap (tyvarbndr . snd) tyargs

    -- Get deriving clauses
    let (inlineDerives, mkStandaloneDerives) = getDerivesFor routeOpts cxt

    -- Create the datatype declaration
    let dataDecl = DataD [] targetName subrouteDecTypeArgs Nothing cons inlineDerives

    -- Compute ParentArgs type from accumulated dynamic pieces
    let piecesAndNames =
            map (\p -> case p of
                    Static str -> Left str
                    Dynamic a -> Right a)
                prepieces
        preDyns = mapMaybe (either (const Nothing) Just) piecesAndNames

    parentDynT <-
        case preDyns of
            [] -> [t| () |]
            [t] -> pure t
            ts -> pure $ foldl' AppT (TupleT (length ts)) ts

    -- Generate variable names for parent pieces
    parentNames <- forM piecesAndNames $ \epiece'name -> do
        case epiece'name of
            Left piece ->
                pure (Left piece)
            Right t -> do
                let tyName = filter isAlphaNum $ show t
                    lowerFirst xs = case xs of
                        (a : as) -> toLower a : as
                        [] -> error "empty name????"
                Right <$> newName (lowerFirst tyName)

    -- Generate renderRouteNested clauses for the children
    (childClauses, _childNames) <- mkRenderRouteNestedClauses parentNames ress

    -- Create the RenderRouteNested instance
    parentSiteT <- [t| ParentSite $(pure targetDataType) |]
    parentDynSig <- [t| ParentArgs $(pure targetDataType) |]
    let renderRouteNestedInstance =
            InstanceD
                Nothing
                (cxt <> getRequiredContextFor routeOpts (fst <$> tyargs))
                (ConT ''RenderRouteNested `AppT` targetDataType)
                [ TySynInstD $ TySynEqn Nothing parentSiteT typ
                , TySynInstD $ TySynEqn Nothing parentDynSig parentDynT
                , FunD 'renderRouteNested childClauses
                ]

    -- Return the datatype declaration, instance, and any standalone derives
    return $ dataDecl : renderRouteNestedInstance : mkStandaloneDerives targetDataType
  where
    mkRouteConsOpts' :: [Piece Type] -> [ResourceTree Type] -> Q ([Con], [Dec])
    mkRouteConsOpts' prePieces = foldMap (mkRouteCon' prePieces)

    mkRouteCon' :: [Piece Type] -> ResourceTree Type -> Q ([Con], [Dec])
    mkRouteCon' _ (ResourceLeaf res) = do
        pure ([con], [])
      where
        con = NormalC (mkName $ resourceName res)
            $ map (notStrict,)
            $ concat [singles, multi, sub]
        singles = concatMap toSingle $ resourcePieces res
        toSingle Static{} = []
        toSingle (Dynamic typ') = [typ']

        multi = maybeToList $ resourceMulti res

        sub =
            case resourceDispatch res of
                Subsite { subsiteType = subtyp } -> [ConT ''Route `AppT` subtyp]
                _ -> []

    mkRouteCon' prePieces (ResourceParent name _check pieces children) = do
        -- For nested parents within the focused route, recursively generate
        let accumulatedPieces = prePieces <> pieces
        (cons, decs) <- mkRouteConsOpts' accumulatedPieces children
        let childDataName = mkName name

        -- Check if the child datatype already exists
        mname' <- lookupTypeName name
        mdec <- case mname' of
            Just _ -> pure Nothing
            Nothing -> do
                -- Generate the child datatype and instances
                let tyvarbndr' =
#if MIN_VERSION_template_haskell(2,21,0)
                        (`PlainTV` BndrReq) :: Name -> TyVarBndr BndrVis
#elif MIN_VERSION_template_haskell(2,17,0)
                        (`PlainTV` ()) :: Name -> TyVarBndr ()
#else
                        PlainTV :: Name -> TyVarBndr
#endif
                    subrouteDecTypeArgs' = fmap (tyvarbndr' . snd) tyargs
                    (inlineDerives', mkStandaloneDerives') = getDerivesFor routeOpts cxt
                    childData = DataD [] childDataName subrouteDecTypeArgs' Nothing cons inlineDerives'
                    childDataType = foldl' AppT (ConT childDataName) (fst <$> tyargs)

                -- Generate the RenderRouteNested instance for the child
                let piecesAndNames' =
                        map (\p -> case p of
                                Static str -> Left str
                                Dynamic a -> Right a)
                            (prePieces <> pieces)
                    preDyns' = mapMaybe (either (const Nothing) Just) piecesAndNames'

                parentDynT' <-
                    case preDyns' of
                        [] -> [t| () |]
                        [t] -> pure t
                        ts -> pure $ foldl' AppT (TupleT (length ts)) ts

                parentNames' <- forM piecesAndNames' $ \epiece'name -> do
                    case epiece'name of
                        Left piece -> pure (Left piece)
                        Right t -> do
                            let tyName = filter isAlphaNum $ show t
                                lowerFirst xs = case xs of
                                    (a : as) -> toLower a : as
                                    [] -> error "empty name????"
                            Right <$> newName (lowerFirst tyName)

                (childClauses', _) <- mkRenderRouteNestedClauses parentNames' children

                parentSiteT' <- [t| ParentSite _ |]
                parentDynSig' <- [t| ParentArgs _ |]
                let childInstances =
                        InstanceD
                            Nothing
                            (cxt <> getRequiredContextFor routeOpts (fst <$> tyargs))
                            (ConT ''RenderRouteNested `AppT` childDataType)
                            [ TySynInstD $ TySynEqn Nothing parentSiteT' typ
                            , TySynInstD $ TySynEqn Nothing parentDynSig' parentDynT'
                            , FunD 'renderRouteNested childClauses'
                            ]

                pure $ Just (childData : [childInstances] ++ mkStandaloneDerives' childDataType)

        let con = NormalC (mkName name)
                $ map (notStrict,)
                $ singles ++ [foldl' (\b a -> b `AppT` fst a) (ConT childDataName) tyargs]

            singles = concatMap toSingle pieces
            toSingle Static{} = []
            toSingle (Dynamic subtyp) = [subtyp]

        return ([con], maybe id (<>) mdec decs)
