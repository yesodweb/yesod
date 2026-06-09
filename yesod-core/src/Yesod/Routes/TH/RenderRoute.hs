{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Yesod.Routes.TH.RenderRoute
    ( -- ** RenderRoute
      mkRenderRouteInstanceOpts
    , mkRouteConsOpts
    , shouldCreateResources

    , RouteOpts
    , defaultOpts
    , setEqDerived
    , setShowDerived
    , setReadDerived
    , setCreateResources
    , setFocusOnNestedRoute
    , roFocusOnNestedRoute
    , roNestedRouteFallthrough
    , setParameterizedSubroute
    , setNestedRouteFallthrough
    , nullifyWhenNoParam
    , DiscoveryMode(..)
    , discoveryMode
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
import Data.Char
import Yesod.Core.Class.Dispatch.ToParentRoute
import Yesod.Core.Class.Dispatch
import Yesod.Core.Handler
import Data.Proxy
import Yesod.Core.Class.Yesod

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
    , roCreateResources :: Bool
    , roFocusOnNestedRoute :: Maybe String
    -- ^ If this option is set, then we will only generate datatypes for
    -- the nested subroute that matches this string.
    --
    -- @since 1.7.0.0
    , roParameterizedSubroute :: Bool
    , roNestedRouteFallthrough :: Bool
    -- ^ If 'True', then a nested route will fall through if it fails to
    -- match. If 'False', then a nested route will throw 'notFound' if it
    -- does not match.
    --
    -- Default: 'False'.
    --
    -- @since 1.7.0.0
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
-- @since 1.7.0.0
setFocusOnNestedRoute :: Maybe String -> RouteOpts -> RouteOpts
setFocusOnNestedRoute mstr rdo = rdo { roFocusOnNestedRoute = mstr }

-- | When 'True', a nested route can fall through if it does not match.
--
-- @since 1.7.0.0
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

-- | If True, generate nested-discovery code (separate subroute datatypes and
-- @RenderRouteNested@ / dispatch instances) for a /parameterized/ site instead
-- of the backwards-compatible inline output. The subroute datatypes then carry
-- the parent site's type variables so that the @ParentSite@ \/ @ParentArgs@
-- associated types stay well-scoped.
--
-- Because the generated nested instances are parameterized over the site's
-- type variables, the splice emits instance heads with non-variable arguments;
-- a module using this typically needs @FlexibleContexts@, @FlexibleInstances@,
-- @MultiParamTypeClasses@, @TypeFamilies@ and, for a parameterized subsite
-- whose @master@ is determined by the @subsite@ (a @subsite -> master@
-- functional dependency on the user's class), @UndecidableInstances@.
--
-- Monomorphic sites always use nested discovery regardless of this flag; see
-- 'discoveryMode'.
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

-- | How a route datatype's children are generated.
--
-- @since 1.7.0.0
data DiscoveryMode
    = -- | The backwards-compatible default for a /parameterized/ site that has
      -- not opted in: subroute datatypes stay unparameterized (kind 'Type')
      -- and @renderRoute@ \/ @parseRoute@ \/ dispatch inline the children,
      -- exactly as pre-nested-discovery Yesod did. Generating the discovery
      -- machinery here would put the parent's type variables out of scope on
      -- the child datatype, so we don't.
      InlineCompat
    | -- | Generate the "nested route discovery" machinery: parameterized
      -- subroute datatypes plus the @RenderRouteNested@ \/ @ParseRouteNested@
      -- \/ @YesodDispatchNested@ \/ @ToParentRoute@ \/ @UrlToDispatch@ \/
      -- @RedirectUrl@ instances and the delegating method bodies that go with
      -- them.
      NestedDiscovery
    deriving (Eq, Show)

-- | Classify how a route datatype's children should be generated.
--
-- The 'Bool' argument is whether the route datatype has type arguments (i.e.
-- 'hasTyArgs' of its 'TyArgs' — 'False' for a monomorphic site like @App@).
-- Mode and tyargs are kept as separate facts: a monomorphic site is
-- 'NestedDiscovery' even though its 'TyArgs' is 'NoTyArgs', so the path must
-- not be inferred from the absence of type arguments alone.
--
-- 'NestedDiscovery' is chosen when any of the following hold:
--
--   * The site has /no/ type parameters. Subroute datatypes then need no type
--     variables, so the machinery is always safe, and this preserves the
--     historical behaviour of monomorphic sites (including splitting nested
--     routes across modules).
--
--   * Parameterized subroutes were explicitly requested
--     ('setParameterizedSubroute'). The subroute datatypes then carry the
--     parent's type variables so the @ParentSite@ \/ @ParentArgs@ associated
--     types are well-scoped.
--
--   * We are focusing on a nested route for module-splitting
--     ('setFocusOnNestedRoute').
--
-- @since 1.7.0.0
discoveryMode :: RouteOpts -> Bool -> DiscoveryMode
discoveryMode opts hasArgs
    | not hasArgs                        = NestedDiscovery
    | roParameterizedSubroute opts       = NestedDiscovery
    | isJust (roFocusOnNestedRoute opts) = NestedDiscovery
    | otherwise                          = InlineCompat

-- | 'PlainTV' across template-haskell versions. @th-abstraction@ covers this,
-- but the version it was introduced in isn't always available — and the
-- 'TyVarBndr' /flag/ type also varies by version, so the result type is
-- CPP-guarded too. Used to build the type-variable binders of generated
-- subroute datatypes.
plainTVCompat :: Name ->
#if MIN_VERSION_template_haskell(2,21,0)
    TyVarBndr BndrVis
#elif MIN_VERSION_template_haskell(2,17,0)
    TyVarBndr ()
#else
    TyVarBndr
#endif
plainTVCompat =
#if MIN_VERSION_template_haskell(2,21,0)
    (`PlainTV` BndrReq)
#elif MIN_VERSION_template_haskell(2,17,0)
    (`PlainTV` ())
#else
    PlainTV
#endif

-- | Generate the constructors of a route data type, with custom
-- 'RouteOpts'.
--
-- The first element in the return is the list of constructors for the
-- @Route site@ datatype. The second element is the list of 'Dec' to
-- declare nested datatypes or class instances of the 'RenderRouteNested'
-- for those instances.
--
-- @since 1.6.25.0
-- | Build the data constructor for a route leaf: @LeafR ty… [multi] [Route Sub]@.
-- Shared by the full-tree ('mkRouteConsOpts') and focused-nested
-- ('mkRenderRouteNestedInstanceOpts') constructor generators, which previously
-- spelled this identically.
leafRouteCon :: Resource Type -> Con
leafRouteCon res =
    NormalC (mkName $ resourceName res)
        $ map (notStrict,)
        $ concat [singles, multi, sub]
  where
    singles = concatMap toSingle $ resourcePieces res
    toSingle Static{}      = []
    toSingle (Dynamic typ) = [typ]
    multi = maybeToList $ resourceMulti res
    sub =
        case resourceDispatch res of
            Subsite { subsiteType = typ } -> [ConT ''Route `AppT` typ]
            _ -> []

-- | Build the data constructor for a route parent: @ParentR ty… (ChildR tyargs…)@,
-- whose trailing field is the (possibly type-argument-applied) child route
-- datatype. Shared by the full-tree and focused-nested constructor generators.
parentRouteCon :: String -> [Piece Type] -> TyArgs -> Con
parentRouteCon name pieces tyargs =
    NormalC (mkName name)
        $ map (notStrict,)
        $ singles ++ [applyTyArgs (ConT (mkName name)) tyargs]
  where
    singles = concatMap toSingle pieces
    toSingle Static{}      = []
    toSingle (Dynamic typ) = [typ]

mkRouteConsOpts :: RouteOpts -> Cxt -> TyArgs -> Type -> [ResourceTree Type] -> Q ([Con], [Dec])
mkRouteConsOpts opts cxt origTyargs master resourceTrees = do
    (prePieces, focusedTrees) <-
        case roFocusOnNestedRoute opts of
            Nothing ->
                pure ([], resourceTrees)
            Just target -> do
                let mnestedRoute = findNestedRoute target resourceTrees
                case mnestedRoute of
                    Nothing ->
                        fail $ "Target '" <> target <> "' was not found in resources."
                    Just r ->
                        pure r
    mkRouteConsOpts' prePieces focusedTrees
  where
    -- When nested route discovery is enabled, child datatypes carry the
    -- parent's type variables, because RenderRouteNested has associated type
    -- families (ParentSite, ParentArgs) that reference the parent type;
    -- without the type variables on the child datatype, they'd be out of
    -- scope on the RHS of the type family instance.
    --
    -- When it is disabled (the backwards-compatible default), child
    -- datatypes are unparameterized (kind 'Type'), matching the historical
    -- output. See 'discoveryMode'. Computed once and reused below.
    mode = discoveryMode opts (hasTyArgs origTyargs)
    tyargs =
        case mode of
            NestedDiscovery -> origTyargs
            InlineCompat    -> NoTyArgs
    subrouteDecTypeArgs = fmap plainTVCompat (tyArgsBinders tyargs)

    -- Derives for the child (subroute) datatypes. When the child datatype is
    -- unparameterized (the backwards-compatible default), it cannot carry the
    -- instance context's type variables, so deriving with that context would
    -- make them ambiguous. Mirror master and nullify the context in that case;
    -- keep it only when the child actually carries the type arguments.
    childCxt
        | not (hasTyArgs tyargs) = []
        | otherwise              = cxt
    (inlineDerives, mkStandaloneDerives) = getDerivesFor opts childCxt

    mkRouteConsOpts' :: [Piece Type] -> [ResourceTree Type] -> Q ([Con], [Dec])
    mkRouteConsOpts' prePieces trees = do
        results <- mapM (mkRouteCon prePieces) trees
        pure (mconcat results)

    mkRouteCon :: [Piece Type] -> ResourceTree Type -> Q ([Con], [Dec])
    mkRouteCon _ (ResourceLeaf res) =
        -- A leaf is always a constructor of the (possibly focused) datatype:
        -- in focus mode we have already narrowed @resourceTrees@ to the
        -- target's children, so every leaf reached here is a leaf of the
        -- focused route.
        pure ([leafRouteCon res], [])

    mkRouteCon prePieces (ResourceParent name _check _attrs pieces children) = do
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
                let childData = mkChildDataGen childDataName cons inlineDerives
                    childDataType = applyTyArgs (ConT childDataName) tyargs
                case roFocusOnNestedRoute opts of
                    Just target | target /= name ->
                        -- If we have a target, and this ain't it, don't
                        -- generate
                        pure Nothing
                    _ | InlineCompat <- mode ->
                        -- Backwards-compatible default: emit just the
                        -- (unparameterized) child datatype and its standalone
                        -- derives. No RenderRouteNested instance; renderRoute
                        -- inlines the children.
                        pure $ Just (childData : mkStandaloneDerives childDataType)
                    _ ->
                        -- Nested discovery: either `Just target | target ==
                        -- name` (matching a target, and this is it) or
                        -- `Nothing` (no target, or we already found it). Emit
                        -- the child datatype plus its RenderRouteNested
                        -- instance so the parent's renderRoute can delegate.
                        Just <$>
                            nestedChildDataAndInstance
                                opts cxt childCxt tyargs master childDataName cons
                                (prePieces <> pieces) children

        return ([parentRouteCon name pieces tyargs], maybe id (<>) mdec decs)
      where
        mkChildDataGen :: Name -> [Con] -> [DerivClause] -> Dec
        mkChildDataGen childDataName cons conts =
            DataD [] childDataName subrouteDecTypeArgs Nothing cons conts

-- | The shared preamble for rendering a 'ResourceParent': fresh names for the
-- parent's dynamic pieces and for the wrapped @child@ route, plus the
-- constructor pattern @Name dyn… child@ binding them. Returns the dynamic
-- names, the child name, and the pattern.
parentConPat :: String -> [Piece a] -> Q ([Name], Name, Pat)
parentConPat name pieces = do
    let cnt = length [() | Dynamic{} <- pieces]
    dyns <- replicateM cnt $ newName "dyn"
    child <- newName "child"
    pure (dyns, child, conPCompat (mkName name) $ map VarP $ dyns ++ [child])

-- | Clauses for the 'renderRoute' method. This should be called from the
-- instance derivation for 'RenderRoute'.
mkRenderRouteClauses :: RouteOpts -> TyArgs -> [ResourceTree Type] -> Q [Clause]
mkRenderRouteClauses opts origTyargs =
    goList
  where
    goList = fmap mconcat . mapM go

    mode = discoveryMode opts (hasTyArgs origTyargs)

    isDynamic Dynamic{} = True
    isDynamic _ = False

    go (ResourceParent name _check _attrs pieces children) =
        case mode of
            -- Opted in to nested discovery: the parent's renderRoute delegates
            -- to the child's RenderRouteNested instance.
            NestedDiscovery -> goNested
            -- Backwards-compatible default: inline the child clauses,
            -- prepending this parent's path pieces, exactly as historical
            -- Yesod did.
            InlineCompat -> goInline
      where
        goNested = do
            (dyns, child, pat) <- parentConPat name pieces

            let parentArgs = parentArgsExpr dyns
            let renderRouteNestedCall =
                    VarE 'renderRouteNested `AppE` parentArgs `AppE` VarE child

            pure [Clause [pat] (NormalB renderRouteNestedCall) []]

        goInline = do
            (dyns, child, pat) <- parentConPat name pieces

            pack' <- [|pack|]
            tsp <- [|toPathPiece|]
            piecesSingle <- mkPieces (AppE pack' . LitE . StringL) tsp pieces dyns

            childRender <- newName "childRender"
            childClauses <- goList children

            body <- delegatingBody piecesSingle (VarE childRender) (VarE child)

            pure [Clause [pat] (NormalB body) [FunD childRender childClauses]]

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
        piecesSingle <- mkPieces (AppE pack' . LitE . StringL) tsp (resourcePieces res) dyns

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
                    delegatingBody piecesSingle rr (VarE x)
                _ -> do
                    colon <- [|(:)|]
                    let cons a b = InfixE (Just a) colon (Just b)
                    return $ mkTupE [foldr cons piecesMulti piecesSingle, ListE []]

        return [Clause [pat] (NormalB body) []]

    mkPieces _ _ [] _ = pure []
    mkPieces toText tsp (Static s:ps) dyns = (toText s :) <$> mkPieces toText tsp ps dyns
    mkPieces toText tsp (Dynamic{}:ps) (d:dyns) = (tsp `AppE` VarE d :) <$> mkPieces toText tsp ps dyns
    mkPieces _ _ (Dynamic _ : _) [] =
        fail "RenderRoute.mkPieces: a dynamic path piece has no corresponding bound variable (route definition and piece-binder list are out of sync)"

-- | Build a renderRoute body that delegates its tail to another render
-- function — either a nested child's @renderRouteNested@ (the inline
-- 'ResourceParent' arm) or an embedded subsite's @renderRoute@ (the
-- 'ResourceLeaf' subsite arm). The path-piece list passed in is prepended to
-- the delegate's path list (callers include any parent pieces themselves) and
-- the query string is threaded through unchanged.
delegatingBody
    :: [Exp]    -- ^ this route's own path pieces (already including any parent pieces)
    -> Exp      -- ^ the delegate render function
    -> Exp      -- ^ the child/subsite route argument to render
    -> Q Exp
delegatingBody piecesSingle rr childArg = do
    a <- newName "a"
    b <- newName "b"
    colon <- [|(:)|]
    let cons y ys = InfixE (Just y) colon (Just ys)
        pieces' = foldr cons (VarE a) piecesSingle
    pure $ LamE [TupP [VarP a, VarP b]] (mkTupE [pieces', VarE b])
        `AppE` (rr `AppE` childArg)

-- | Like 'mkRenderRouteClauses', but instead generates clauses for the
-- definition of 'renderRouteNested'.
--
-- @since 1.7.0.0
mkRenderRouteNestedClauses
    :: [Either String Name]
    -- ^ Either the static path piece or the names of the tuple of the parent arg.
    --
    -- These are both necessary to re-synthesize the total route.
    -> [ResourceTree Type]
    -> Q [Clause]
mkRenderRouteNestedClauses parentArgsNames resources = do
    fmap mconcat . mapM go $ resources
  where
    isDynamic Dynamic{} = True
    isDynamic _ = False

    go (ResourceParent name _check _attrs pieces _children) = do
        (dyns, child, pat) <- parentConPat name pieces

        -- Extract parent dynamic variables from parentArgsNames
        let parentDyns = mapMaybe (either (const Nothing) Just) parentArgsNames

        -- Accumulate parent args: combine parent dynamic vars with this route's dynamic vars
        let allDyns = parentDyns ++ dyns
            accumulatedParentArgsExp = parentArgsExpr allDyns

        -- The body should call renderRouteNested with accumulated parent args
        let renderRouteNestedCall =
                VarE 'renderRouteNested `AppE` accumulatedParentArgsExp `AppE` VarE child

        pure [Clause [parentArgsPat parentDyns, pat] (NormalB renderRouteNestedCall) []]

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

        -- Build parent path pieces from parentArgsNames
        parentPieces <- mkParentPieces pack' tsp parentArgsNames parentDyns
        -- Build this resource's own pieces
        thisResourcePieces <- mkPieces (AppE pack' . LitE . StringL) tsp (resourcePieces res) dyns

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
                    -- Combine parent pieces with this resource's pieces
                    delegatingBody (parentPieces ++ thisResourcePieces) rr (VarE x)
                _ -> do
                    colon <- [|(:)|]
                    let cons a b = InfixE (Just a) colon (Just b)
                    -- Combine parent pieces with this resource's pieces
                    let allPieces = parentPieces ++ thisResourcePieces
                    return $ mkTupE [foldr cons piecesMulti allPieces, ListE []]

        return [Clause [parentArgsPat parentDyns, pat] (NormalB body) []]

    mkPieces _ _ [] _ = pure []
    mkPieces toText tsp (Static s:ps) dyns = (toText s :) <$> mkPieces toText tsp ps dyns
    mkPieces toText tsp (Dynamic{}:ps) (d:dyns) = (tsp `AppE` VarE d :) <$> mkPieces toText tsp ps dyns
    mkPieces _ _ (Dynamic _ : _) [] =
        fail "RenderRoute.mkPieces: a dynamic path piece has no corresponding bound variable (route definition and piece-binder list are out of sync)"

    -- Build path pieces from parentArgsNames (which contains both static and dynamic pieces)
    mkParentPieces :: Exp -> Exp -> [Either String Name] -> [Name] -> Q [Exp]
    mkParentPieces pack' tsp parentArgsNames' parentDyns = goParentPieces parentArgsNames' parentDyns
      where
        goParentPieces [] _ = pure []
        goParentPieces (Left staticStr : rest) dyns =
            ((pack' `AppE` LitE (StringL staticStr)) :) <$> goParentPieces rest dyns
        goParentPieces (Right _ : rest) (dyn : dyns) =
            ((tsp `AppE` VarE dyn) :) <$> goParentPieces rest dyns
        goParentPieces (Right _ : _) [] =
            fail "RenderRoute.mkParentPieces: a dynamic parent path piece has no corresponding bound variable"

-- | The shared core of every 'RenderRouteNested' instance body. From a parent
-- route's accumulated path pieces and its child resources it computes the
-- @ParentArgs@ tuple 'Type' and the @renderRouteNested@ method clauses. The
-- three instance-emitting sites (the inline child datatype in
-- 'mkRenderRouteInstanceOpts', the focused target in
-- 'mkRenderRouteNestedInstanceOpts', and that target's nested children) differ
-- only in the instance head, the @ParentSite@ right-hand side, and the context
-- they wrap around this — so only those wrappers remain at the call sites, and
-- the piece-naming\/clause-building boilerplate (including the dynamic-binder
-- name derivation) lives here once.
renderRouteNestedBody
    :: [Piece Type]            -- ^ accumulated parent path pieces
    -> [ResourceTree Type]     -- ^ child resources
    -> Q (Type, [Clause])      -- ^ (ParentArgs tuple type, renderRouteNested clauses)
renderRouteNestedBody prepieces children = do
    let piecesAndNames =
            map (\p -> case p of
                    Static str -> Left str
                    Dynamic a -> Right a)
                prepieces
        preDyns = mapMaybe (either (const Nothing) Just) piecesAndNames
    let parentDynT = parentArgsType preDyns
    parentNames <- forM piecesAndNames $ \epiece'name ->
        case epiece'name of
            Left piece -> pure (Left piece)
            Right t ->
                case filter isAlphaNum (show t) of
                    (a : as) -> Right <$> newName (toLower a : as)
                    []       -> fail
                        "renderRouteNested: a dynamic piece's type renders with no alphanumeric characters, so no binder name can be derived from it"
    childClauses <- mkRenderRouteNestedClauses parentNames children
    pure (parentDynT, childClauses)

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
    -> TyArgs
    -- ^ Type arguments
    -> Type
    -- ^ The type of the foundation resource
    -> [ResourceTree Type]
    -- ^ The actual tree of routes to generate code for
    -> Q [Dec]
mkRenderRouteInstanceOpts opts cxt tyargs typ ress = do
    case roFocusOnNestedRoute opts of
        Nothing -> do
            cls <- mkRenderRouteClauses opts tyargs ress
            (cons, decs) <- mkRouteConsOpts opts cxt tyargs typ ress
            let did = DataInstD []
#if MIN_VERSION_template_haskell(2,15,0)
                    Nothing routeDataName
#else
                    ''Route [typ]
#endif
                    Nothing cons inlineDerives
            -- ToParentRoute instances are only needed by the nested-discovery
            -- machinery; the backwards-compatible default emits none.
            parentRouteInstancesDecs <-
                case discoveryMode opts (hasTyArgs tyargs) of
                    NestedDiscovery -> mkToParentRouteInstances opts cxt tyargs ress
                    InlineCompat    -> pure []
            pure $ mconcat
                [ pure $ instanceD cxt (ConT ''RenderRoute `AppT` typ)
                    [ did
                    , FunD 'renderRoute cls
                    ]
                , mkStandaloneDerives routeDataName
                , decs
                , parentRouteInstancesDecs
                ]
        Just target ->
            case findNestedRoute target ress of
                Nothing ->
                    fail $ "Target '" <> target <> "' was not found in resources."
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
mkToParentRouteInstances :: RouteOpts -> Cxt -> TyArgs -> [ResourceTree  Type] -> Q [Dec]
mkToParentRouteInstances routeOpts cxt origTyargs ress = do
    mconcat <$> mapM (go ([], [])) ress
  where
    go _ (ResourceLeaf _) =
        pure []
    go (accPieces, parentConstructors) (ResourceParent name _check _attrs pieces children) = do
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

        -- Build the full route by applying all parent constructors
        -- We need to partition allParentDynVars according to each constructor's piece count
        let applyConToParentArgs = buildRouteExpr parentConstructors (mkName name) allParentDynVars (VarE child)

        let toParentRouteD =
                FunD 'toParentRoute
                    [ Clause
                        [parentArgsPat allParentDynVars, VarP child]
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
        applyTyArgs (ConT (mkName name)) origTyargs

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

notStrict :: Bang
notStrict = Bang NoSourceUnpackedness NoSourceStrictness

-- | Assemble a @RenderRouteNested@ instance from the pieces that vary between
-- the three sites that emit one (the inline child datatype in
-- 'mkRouteConsOpts', the focused target and its nested children in
-- 'mkRenderRouteNestedInstanceOpts'). The context, child route 'Type',
-- @ParentSite@ right-hand side (the foundation 'Type') and @ParentArgs@ tuple
-- 'Type' plus the @renderRouteNested@ clauses are all that differ; everything
-- else is shared here.
--
-- @since 1.7.0.0
mkRenderRouteNestedInstanceD
    :: Cxt        -- ^ instance context
    -> Type       -- ^ the child route datatype (instance head argument)
    -> Type       -- ^ the @ParentSite@ right-hand side (the foundation type)
    -> Type       -- ^ the @ParentArgs@ tuple type
    -> [Clause]   -- ^ the @renderRouteNested@ method clauses
    -> Dec
mkRenderRouteNestedInstanceD instCxt childRouteType parentSiteRHS parentArgsT clauses =
    instanceD
        instCxt
        (ConT ''RenderRouteNested `AppT` childRouteType)
        [ TySynInstD $ TySynEqn Nothing (ConT ''ParentSite `AppT` childRouteType) parentSiteRHS
        , TySynInstD $ TySynEqn Nothing (ConT ''ParentArgs `AppT` childRouteType) parentArgsT
        , FunD 'renderRouteNested clauses
        ]

-- | Generate the declarations for a nested-discovery /child/ datatype: the
-- (parameterized) datatype declaration, its @RenderRouteNested@ instance, and
-- any standalone deriving clauses. Shared by every nested-discovery
-- constructor generator ('mkRouteConsOpts' and 'mkRenderRouteNestedInstanceOpts')
-- so the datatype shape, instance context, and @ParentSite@\/@ParentArgs@ wiring
-- stay in one place. The focus\/inline gating that decides /whether/ to emit
-- these stays at the call sites, since that differs between them.
--
-- @since 1.7.0.0
nestedChildDataAndInstance
    :: RouteOpts
    -> Cxt           -- ^ instance context for the @RenderRouteNested@ instance
    -> Cxt           -- ^ deriving context for the child datatype (nullified when
                     --   the child carries no type arguments; see 'mkRouteConsOpts')
    -> TyArgs        -- ^ type arguments carried by the child datatype
    -> Type          -- ^ the @ParentSite@ right-hand side (the foundation type)
    -> Name          -- ^ the child datatype name
    -> [Con]         -- ^ the child datatype's constructors
    -> [Piece Type]  -- ^ accumulated parent path pieces for this child
    -> [ResourceTree Type] -- ^ the child's own children
    -> Q [Dec]
nestedChildDataAndInstance opts instCxt derivCxt tyargs parentSiteRHS childDataName cons accPieces children = do
    let subrouteDecTypeArgs = fmap plainTVCompat (tyArgsBinders tyargs)
        (inlineDerives, mkStandaloneDerives) = getDerivesFor opts derivCxt
        childData = DataD [] childDataName subrouteDecTypeArgs Nothing cons inlineDerives
        childDataType = applyTyArgs (ConT childDataName) tyargs
    (parentDynT, childClauses) <- renderRouteNestedBody accPieces children
    let childInstance =
            mkRenderRouteNestedInstanceD instCxt childDataType parentSiteRHS parentDynT childClauses
    pure (childData : childInstance : mkStandaloneDerives childDataType)

mkRenderRouteNestedInstanceOpts
    :: RouteOpts
    -> Cxt
    -> TyArgs
    -> Type
    -> [Piece Type]
    -> String
    -> [ResourceTree Type]
    -> Q [Dec]
mkRenderRouteNestedInstanceOpts routeOpts cxt tyargs typ prepieces target ress = do
    -- Generate constructors for all children
    (cons, childDecs) <- mkRouteConsOpts' prepieces ress

    let targetName = mkName target
        targetDecs =
            -- The focused target datatype and its RenderRouteNested instance
            -- are emitted through the same shared builder as the in-module
            -- and nested-child paths, so the instance context is plain @cxt@
            -- (matching the in-module generator); demanding extra
            -- Eq/Show/Read constraints on the site type arguments here would
            -- be undischargeable at the delegating RenderRoute instance.
            nestedChildDataAndInstance
                routeOpts cxt childCxt tyargs typ targetName cons prepieces ress

    -- Return the datatype declaration, instance, and any standalone derives,
    -- followed by the declarations for any nested children.
    (<> childDecs) <$> targetDecs
  where
    -- The focused datatype carries the site's type arguments, so its deriving
    -- context only makes sense when it actually has any (mirroring the
    -- in-module 'mkRouteConsOpts' nullification).
    childCxt
        | not (hasTyArgs tyargs) = []
        | otherwise              = cxt

    mkRouteConsOpts' :: [Piece Type] -> [ResourceTree Type] -> Q ([Con], [Dec])
    mkRouteConsOpts' prePieces trees = do
        results <- mapM (mkRouteCon' prePieces) trees
        pure (mconcat results)

    mkRouteCon' :: [Piece Type] -> ResourceTree Type -> Q ([Con], [Dec])
    mkRouteCon' _ (ResourceLeaf res) =
        pure ([leafRouteCon res], [])

    mkRouteCon' prePieces (ResourceParent name _check _attrs pieces children) = do
        -- For nested parents within the focused route, recursively generate
        let accumulatedPieces = prePieces <> pieces
        (cons, decs) <- mkRouteConsOpts' accumulatedPieces children
        let childDataName = mkName name

        -- Check if the child datatype already exists
        mname' <- lookupTypeName name
        mdec <- case mname' of
            Just _ -> pure Nothing
            Nothing ->
                -- Generate the child datatype and its RenderRouteNested
                -- instance through the same shared builder used everywhere
                -- else.
                Just <$>
                    nestedChildDataAndInstance
                        routeOpts cxt childCxt tyargs typ childDataName cons
                        accumulatedPieces children

        return ([parentRouteCon name pieces tyargs], maybe id (<>) mdec decs)
