{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Yesod.Routes.TH.Dispatch
    ( MkDispatchSettings (..)
    , mkDispatchClause
    , defaultGetHandler
    , SDC(..)
    , mkDispatchInstance
    , mkNestedDispatchInstance
    , mkNestedSubDispatchInstance
    , NestedDispatchConfig (..)
    , SubsiteEnvMode (..)
    , topLevelNestedConfig
    , subsiteNestedConfig
    , mkMDS
    , mkYesodSubDispatch
    , subTopDispatch
    , parseYesodName
    ) where

import Text.Parsec (parse, many1, many, eof, try, option, sepBy1)
import Text.ParserCombinators.Parsec.Char (alphaNum, spaces, string, char)
import Data.Maybe
import Data.Proxy (Proxy(..))
import Yesod.Routes.TH.RenderRoute
import qualified Network.Wai as W
import Yesod.Core.Content (ToTypedContent (..))
import Language.Haskell.TH hiding (cxt, instanceD)
import Yesod.Core.Types hiding (Body)
import Yesod.Core.Class.Dispatch
import Prelude hiding (exp)
import Yesod.Routes.TH.Internal
import Web.PathPieces
import Control.Monad
import Data.List (foldl')
import Yesod.Routes.TH.Types
import Data.Char (toLower)
import Yesod.Core.Internal.Run
import Yesod.Core.Handler
import Yesod.Core.Class.Dispatch.ToParentRoute (ToParentRoute(..))
import Yesod.Core.Class.Yesod (Yesod)

-- | This datatype describes how to create the dispatch clause for a route
-- path.
data MkDispatchSettings b site c = MkDispatchSettings
    { mdsRunHandler :: Q Exp
    , mdsSubDispatcher :: Q Exp
    , mdsGetPathInfo :: Q Exp
    , mdsSetPathInfo :: Q Exp
    , mdsMethod :: Q Exp
    , mds404 :: Q Exp
    , mds405 :: Q Exp
    , mdsGetHandler :: Maybe String -> String -> Q Exp
    , mdsUnwrapper :: Exp -> Q Exp
    , mdsNestedRouteFallthrough :: !Bool
    -- ^ When 'True', fall through if no route matches (except in the final
    -- case). When 'False', return 404 if the current route clause fails to
    -- match.
    --
    -- @since 1.7.0.0
    , mdsNestedDispatchClass :: Name
    -- ^ The class to check for nested dispatch delegation.
    -- @''YesodDispatchNested@ for top-level dispatch,
    -- @''YesodSubDispatchNested@ for subsite dispatch.
    --
    -- @since 1.7.0.0
    , mdsNestedDispatchFn :: Name
    -- ^ The function to call for nested dispatch delegation.
    -- @'yesodDispatchNested@ for top-level dispatch,
    -- @'yesodSubDispatchNested@ for subsite dispatch.
    --
    -- @since 1.7.0.0
    }

-- | Whether 'mkDispatchClause's @go@ is generating a top-level dispatch clause
-- (the final, unwrapped dispatch result) or an inline nested-helper clause (a
-- @'Maybe'@ wrapped in 'Just' so the enclosing parent can fall through on a
-- miss). Internal to the clause builder.
data DispatchPhase = TopLevelDispatch | NestedDispatch

data SDC = SDC
    { clause404 :: Clause
    , extraParams :: [Exp]
    , extraCons :: [Exp]
    , envExp :: Exp
    , reqExp :: Exp
    }

-- | Configuration for generating nested dispatch clauses.
-- Parameterizes the runner function, dispatch function/class, and
-- how subsite environments are constructed.
--
-- @since 1.7.0.0
data NestedDispatchConfig = NestedDispatchConfig
    { ndcRunnerFn      :: Name
    -- ^ Runner function: @'yesodRunner@ for top-level, @'subHelper@ for subsites
    , ndcDispatchFn    :: Name
    -- ^ Nested dispatch function: @'yesodDispatchNested@ or @'yesodSubDispatchNested@
    , ndcDispatchClass :: Name
    -- ^ Nested dispatch class: @''YesodDispatchNested@ or @''YesodSubDispatchNested@
    , ndcSubsiteEnv    :: SubsiteEnvMode
    -- ^ How to construct 'YesodSubRunnerEnv' for child subsites
    }

-- | How to construct 'YesodSubRunnerEnv' when dispatching to a child subsite
-- within a nested route.
--
-- @since 1.7.0.0
data SubsiteEnvMode
    = DirectEnv
    -- ^ Top-level: construct 'YesodSubRunnerEnv' with @yesodRunner@ as parent
    -- runner and the 'YesodRunnerEnv' directly as parent env.
    | ComposedEnv
    -- ^ Subsite: compose through the outer 'YesodSubRunnerEnv', threading
    -- the master site's runner and env through (like 'subTopDispatch').

-- | Configuration for generating 'YesodDispatchNested' instances.
--
-- @since 1.7.0.0
topLevelNestedConfig :: NestedDispatchConfig
topLevelNestedConfig = NestedDispatchConfig
    { ndcRunnerFn      = 'yesodRunner
    , ndcDispatchFn    = 'yesodDispatchNested
    , ndcDispatchClass = ''YesodDispatchNested
    , ndcSubsiteEnv    = DirectEnv
    }

-- | Configuration for generating 'YesodSubDispatchNested' instances.
--
-- @since 1.7.0.0
subsiteNestedConfig :: NestedDispatchConfig
subsiteNestedConfig = NestedDispatchConfig
    { ndcRunnerFn      = 'subHelper
    , ndcDispatchFn    = 'yesodSubDispatchNested
    , ndcDispatchClass = ''YesodSubDispatchNested
    , ndcSubsiteEnv    = ComposedEnv
    }

-- | A simpler version of Yesod.Routes.TH.Dispatch.mkDispatchClause, based on
-- view patterns.
--
-- The function returns the 'Clause' for the dispatch, along with
-- a @['Name']@ corresponding to the names of types that require generating
-- instances for delegation classes. For 'ParseRoute', that's
-- 'ParseRouteNtested'. For 'YesodDispatch', that's 'YesodDispatchNested'.
-- Since 1.4.0
mkDispatchClause :: forall a b site c. [Name] -> [Exp] -> TyArgs -> MkDispatchSettings b site c -> [ResourceTree a] -> Q ([String], Clause)
mkDispatchClause preDyns parentCons tyargs MkDispatchSettings {..} resources = do
    envName <- newName "env"
    reqName <- newName "req"
    helperName <- newName "dispatchHelper"

    let envE = VarE envName
        reqE = VarE reqName
        helperE = VarE helperName

    clause404' <- mkClause404 envE reqE
    getPathInfo <- mdsGetPathInfo
    let pathInfo = getPathInfo `AppE` reqE

    let sdc = SDC
            { clause404 = clause404'
            , extraParams = []
            , extraCons = parentCons
            , envExp = envE
            , reqExp = reqE
            }
    -- The top-level resources produce the final (unwrapped) dispatch clauses;
    -- the inline children of a parent are generated as 'NestedDispatch' helper
    -- clauses (wrapped in 'Just') by the recursion below.
    (childNames, clauses) <- mconcat <$> mapM (go TopLevelDispatch sdc) resources

    pure
        ( childNames
        , Clause
            [VarP envName, VarP reqName]
            (NormalB $ helperE `AppE` pathInfo)
            [FunD helperName $ clauses ++ [clause404']]
        )
  where
    go :: DispatchPhase -> SDC -> ResourceTree a -> Q ([String], [Clause])
    go goPhase sdc (ResourceParent name _check _attrs pieces children) = do
        -- Delegate to the child's nested-dispatch instance when one already
        -- exists (the configured class: YesodDispatchNested for top-level,
        -- YesodSubDispatchNested for subsites). This is what makes splitting
        -- a parent's nested routes across modules work. When no such instance
        -- exists — the ordinary single-module case — the check fails and we
        -- inline the children below, so this stays backwards compatible.
        -- 'nestedInstanceExists' is the shared probe: it resolves the name once
        -- and saturates by the child's own reified arity, so it never aborts
        -- the splice (see its haddock).
        instanceExists <-
            nestedInstanceExists mdsNestedDispatchClass =<< resolveRouteCon name

        (pats, dyns) <- handlePiecesM newName pieces
        restName <- newName "_rest"
        let restE = VarE restName
            restP = VarP restName

        helperName <- newName $ "helper" ++ name
        let helperE = VarE helperName

        let constr = applyConPieces name dyns
            routeDyns = dyns
            -- ParentArgs for the nested route are the dynamics from THIS route's pieces
            -- plus any accumulated parent dynamics from outer scopes
            thisRouteParentArgs = fmap VarE preDyns <> extraParams sdc ++ dyns

        -- Build helper clauses: delegate to instance if it exists, otherwise inline
        helperClauses <- if instanceExists
            then do
                expr <- nestedDispatchCall mdsNestedDispatchFn name routeDyns tyargs sdc thisRouteParentArgs
                pure [Clause [restP] (NormalB expr) []]
            else do
                -- Inline dispatch: recursively generate clauses for children
                let childSdc = sdc
                        { extraParams = extraParams sdc ++ dyns
                        , extraCons = extraCons sdc ++ [constr]
                        }
                (_childNames, childClauses) <- mconcat <$> mapM (go NestedDispatch childSdc) children
                let fallbackClause = Clause [WildP] (NormalB (ConE 'Nothing)) []
                pure $ childClauses ++ [fallbackClause]

        body <- if mdsNestedRouteFallthrough
            then mkGuardedBody (helperE `AppE` restE) $ \match' -> do
                let result = VarE match'
                case goPhase of
                    TopLevelDispatch -> pure result
                    NestedDispatch -> pure $ ConE 'Just `AppE` result
            else do
                -- When fallthrough is disabled, explicitly handle Nothing with 404
                matchName <- newName "match"
                handler <- mds404
                runHandlerE <- mdsRunHandler
                let baseNotFoundExp = runHandlerE `AppE` handler `AppE` envExp sdc `AppE` ConE 'Nothing `AppE` reqExp sdc
                    matchResult = case goPhase of
                        TopLevelDispatch -> VarE matchName
                        NestedDispatch -> ConE 'Just `AppE` VarE matchName
                    notFoundResult = case goPhase of
                        TopLevelDispatch -> baseNotFoundExp
                        NestedDispatch -> ConE 'Just `AppE` baseNotFoundExp
                return $ NormalB $ CaseE (helperE `AppE` restE)
                    [ Match (conPCompat 'Just [VarP matchName]) (NormalB matchResult) []
                    , Match (conPCompat 'Nothing []) (NormalB notFoundResult) []
                    ]

        pure
            -- Report this parent as needing a nested-dispatch instance only
            -- when we did NOT already delegate to an existing one. Whether the
            -- caller acts on this (i.e. actually generates the instance) is the
            -- caller's decision — see 'mkDispatchInstance'.
            ( if instanceExists
                then mempty
                else [name]
            , [ Clause
                [mkPathPat (EndRest restName) pats]
                body
                [FunD helperName helperClauses]]
            )

    go phase' SDC {..} (ResourceLeaf (Resource name pieces dispatch _ _check)) = do
                (pats, dyns) <- handlePiecesM newName pieces

                (chooseMethod, finalPat) <- handleDispatch dispatch dyns

                -- For nested dispatch, wrap the result in Just
                -- chooseMethod already has type (Response -> IO ResponseReceived) -> IO ResponseReceived
                -- We just wrap it in Just, no need for extra lambda
                wrappedMethod <- case phase' of
                    NestedDispatch -> return $ ConE 'Just `AppE` chooseMethod
                    TopLevelDispatch -> return chooseMethod

                pure
                    ( []
                    , pure $ Clause
                        [mkPathPat finalPat pats]
                        (NormalB wrappedMethod)
                        []
                    )
      where
        handleDispatch :: Dispatch a -> [Exp] -> Q (Exp, PathTail)
        handleDispatch dispatch' dyns =
            case dispatch' of
                Methods multi methods -> do
                    (finalPat, mfinalE) <-
                        case multi of
                            Nothing -> return (EndExact, Nothing)
                            Just _ -> do
                                multiName <- newName "multi"
                                return (EndMulti multiName, Just $ VarE multiName)

                    let dynsMulti =
                            case mfinalE of
                                Nothing -> dyns
                                Just e -> dyns ++ [e]
                        route' = applyConPieces name dynsMulti
                        route = foldr AppE route' extraCons
                        jroute = ConE 'Just `AppE` route
                        allDyns = extraParams ++ dynsMulti
                        mkRunExp mmethod = do
                            runHandlerE <- mdsRunHandler
                            handlerE' <- mdsGetHandler mmethod name
                            handlerE <- mdsUnwrapper $ foldl' AppE handlerE' allDyns
                            return $ runHandlerE
                                `AppE` handlerE
                                `AppE` envExp
                                `AppE` jroute
                                `AppE` reqExp

                    func <-
                        case methods of
                            [] -> mkRunExp Nothing
                            _ -> do
                                getMethod <- mdsMethod
                                let methodE = getMethod `AppE` reqExp
                                matches <- forM methods $ \method -> do
                                    exp <- mkRunExp (Just method)
                                    return $ Match (LitP $ StringL method) (NormalB exp) []
                                match405 <- do
                                    runHandlerE <- mdsRunHandler
                                    handlerE <- mds405
                                    let exp = runHandlerE
                                            `AppE` handlerE
                                            `AppE` envExp
                                            `AppE` jroute
                                            `AppE` reqExp
                                    return $ Match WildP (NormalB exp) []
                                return $ CaseE methodE $ matches ++ [match405]

                    return (func, finalPat)

                Subsite _ getSub -> do
                    restPath <- newName "restPath"
                    setPathInfoE <- mdsSetPathInfo
                    subDispatcherE <- mdsSubDispatcher
                    runHandlerE <- mdsRunHandler
                    let allDyns = extraParams ++ dyns
                    sub2 <- mkLambda "sub" $ \sub ->
                        pure $ foldl' (\a b -> a `AppE` b) (VarE (mkName getSub) `AppE` VarE sub) allDyns
                    route <- mkLambda "sroute" $ \sroute ->
                        pure $ let route' = applyConPieces name dyns
                               in foldr AppE (AppE route' $ VarE sroute) extraCons
                    let reqExp' = setPathInfoE `AppE` VarE restPath `AppE` reqExp
                        exp = subDispatcherE
                            `AppE` runHandlerE
                            `AppE` sub2
                            `AppE` route
                            `AppE` envExp
                            `AppE` reqExp'
                    return (exp, EndRest restPath)

    -- The dispatch helper produced by 'mkDispatchClause' is always a terminal
    -- authority (the top-level / subsite entry point), so a final miss commits
    -- to a 404 here. Inline nested helpers built by the 'NestedDispatch'
    -- recursion bake their own @Nothing@ fallback at their call site instead.
    mkClause404 envE reqE = do
        handler <- mds404
        runHandlerE <- mdsRunHandler
        let exp = runHandlerE `AppE` handler `AppE` envE `AppE` ConE 'Nothing `AppE` reqE
        return $ Clause [WildP] (NormalB exp) []

-- | This function generates code to call the nested dispatch function
-- (either 'yesodDispatchNested' or 'yesodSubDispatchNested').
nestedDispatchCall
    :: Name
    -- ^ The dispatch function to call (e.g., 'yesodDispatchNested or 'yesodSubDispatchNested)
    -> String
    -- ^ The name of the nested route (e.g., "FirstFooR")
    -> [Exp]
    -- ^ The dynamic arguments for this route constructor
    -> TyArgs
    -- ^ Type arguments for parameterized routes
    -> SDC
    -- ^ The accumulated 'SDC'.
    -> [Exp]
    -- ^ The parent dynamic bound variables (for passing as ParentArgs).
    -> Q Exp
nestedDispatchCall dispatchFn routeName routeDyns tyargs sdc parentDyns = do
    -- Look up the type to get its full applied form. When tyargs is
    -- provided, use it; otherwise look up the type's arity and apply
    -- fresh variables (needed for mkYesodSubDispatch which doesn't
    -- know the parent's type args).
    routeType <- appliedRouteTypeNamed routeName tyargs
    let parentDynsExpr = parentArgsExprFromExps parentDyns
        proxyType = SigE (ConE 'Proxy) (AppT (ConT ''Proxy) routeType)
    -- Build the wrapper: \child -> ParentCon (RouteCon d1 d2 child)
    wrapperExpr <- mkLambda "child" $ \childN ->
        pure $ let routeConApp = applyConPieces routeName routeDyns `AppE` VarE childN
               in foldr AppE routeConApp (extraCons sdc)
    pure $ VarE dispatchFn
        `AppE` proxyType
        `AppE` parentDynsExpr
        `AppE` wrapperExpr
        `AppE` envExp sdc
        `AppE` reqExp sdc

-- | Given an 'Exp' which should result in a @'Maybe' a@, does:
--
-- @
--   | Just a <- exp = mkRhs a
-- @
--
mkGuardedBody
    :: Exp
    -- ^ The expression to match Just with
    -> (Name -> Q Exp)
    -- ^ The function to take a 'Name' and create the right hand value on
    -- successful match.
    -> Q Body
mkGuardedBody exp mkRhs = do
    matchName <- newName "match"
    result <- mkRhs matchName
    let patGuard =
            PatG [BindS (conPCompat 'Just [VarP matchName]) exp]
    pure $ GuardedB [(patGuard, result)]

defaultGetHandler :: Maybe String -> String -> Q Exp
defaultGetHandler Nothing s = return $ VarE $ mkName $ "handle" ++ s
defaultGetHandler (Just method) s = return $ VarE $ mkName $ map toLower method ++ s

-- | If the generation of @'YesodDispatch'@ instance require finer
-- control of the types, contexts etc. using this combinator. You will
-- hardly need this generality. However, in certain situations, like
-- when writing library/plugin for yesod, this combinator becomes
-- handy.
mkDispatchInstance
    :: RouteOpts
    -> Type
    -- ^ The master site type
    -> Cxt
    -- ^ Context of the instance
    -> TyArgs
    -- ^ type arguments to constructors
    -> (Exp -> Q Exp)
    -- ^ Unwrap handler
    -> [ResourceTree Type]
    -- ^ The resource
    -> DecsQ
mkDispatchInstance routeOpts master cxt tyargs unwrapper res =
    -- Branch on the focus target via the accessor rather than a constructor
    -- pattern, so 'RouteOpts' can stay abstract (its constructor is not
    -- exported). 'Just target' focuses a single nested route for
    -- module-splitting; 'Nothing' generates the full top-level instance.
    case roFocusOnNestedRoute routeOpts of
        Just target ->
            mkNestedDispatchInstance routeOpts target master cxt tyargs unwrapper res
        Nothing ->
            mkTopLevelDispatchInstance routeOpts master cxt tyargs unwrapper res

mkTopLevelDispatchInstance
    :: RouteOpts
    -> Type
    -> Cxt
    -> TyArgs
    -> (Exp -> Q Exp)
    -> [ResourceTree Type]
    -> DecsQ
mkTopLevelDispatchInstance routeOpts master cxt tyargs unwrapper res = do
    let mds =
            mkMDS
                unwrapper
                [|yesodRunner|]
                [|\parentRunner getSub toParent env -> yesodSubDispatch
                    YesodSubRunnerEnv
                    { ysreParentRunner = parentRunner
                    , ysreGetSub = getSub
                    , ysreToParentRoute = toParent
                    , ysreParentEnv = env
                    }
                |]
        mdsWithNestedDispatch = mds
            { mdsNestedRouteFallthrough = roNestedRouteFallthrough routeOpts
            }
    (childNames, clause') <- mkDispatchClause [] [] tyargs mdsWithNestedDispatch res
    let thisDispatch = FunD 'yesodDispatch [clause']
        -- Only generate 'YesodDispatchNested' instances for children when this
        -- site uses nested discovery. A parameterized site that has not opted
        -- in keeps unparameterized subroute datatypes (see 'discoveryMode' in
        -- RenderRoute), so generating nested instances — which would reference
        -- the parameterized form — must be suppressed to stay consistent.
        childNamesToGenerate =
            case discoveryMode routeOpts (hasTyArgs tyargs) of
                NestedDiscovery -> childNames
                InlineCompat    -> []
    childInstances <-
        fmap mconcat $ forM childNamesToGenerate $ \name -> do
            mkNestedDispatchInstance routeOpts name master cxt tyargs unwrapper res
    return (instanceD cxt yDispatch [thisDispatch] : childInstances)
  where
    yDispatch = ConT ''YesodDispatch `AppT` master

-- | Generate the top-level @YesodDispatchNested@ instance (and the
-- @UrlToDispatch@\/@RedirectUrl@ instances for fragments with no dynamic parent
-- pieces) for a nested route target. A thin wrapper over
-- 'mkNestedDispatchInstanceWith' with the top-level config and the master site.
mkNestedDispatchInstance
    :: RouteOpts
    -> String
    -> Type
    -> Cxt
    -> TyArgs -- ^ tyargs
    -> (Exp -> Q Exp)
    -> [ResourceTree Type]
    -> Q [Dec]
mkNestedDispatchInstance routeOpts target master cxt tyargs unwrapper res =
    mkNestedDispatchInstanceWith topLevelNestedConfig (Just master)
        routeOpts target cxt tyargs unwrapper res

-- | The shared body of the top-level ('mkNestedDispatchInstance') and subsite
-- ('mkNestedSubDispatchInstance') nested-dispatch instance generators. Both
-- find the target, build the parent-dynamics pattern, generate the dispatch
-- clauses via 'genNestedDispatchClauses', emit one
-- @ndcDispatchClass@\/@ndcDispatchFn@ instance, and recurse into nested
-- children. They differ only in the 'NestedDispatchConfig' (which carries the
-- dispatch function\/class and subsite-env mode) and whether a master site is
-- in play: a @'Just' master@ marks the top-level case and additionally emits
-- the @UrlToDispatch@\/@RedirectUrl@ instances, which the subsite case
-- ('Nothing') never produces.
mkNestedDispatchInstanceWith
    :: NestedDispatchConfig
    -> Maybe Type            -- ^ @Just master@ ⇒ top-level (also emit UrlToDispatch\/RedirectUrl); @Nothing@ ⇒ subsite
    -> RouteOpts
    -> String
    -> Cxt
    -> TyArgs
    -> (Exp -> Q Exp)
    -> [ResourceTree Type]
    -> Q [Dec]
mkNestedDispatchInstanceWith config mmaster routeOpts target cxt tyargs unwrapper res = do
    -- Resolve the target subtree from the root exactly once. The recursion into
    -- nested children below threads the already-resolved @(prePieces, subres)@
    -- straight through ('go'), rather than re-passing the full root and walking
    -- it from scratch for every descendant (which was O(nodes × depth)).
    case findNestedRoute target res of
        Nothing ->
            fail $ "Target '" ++ target ++ "' was not found in resources."
        Just (prePieces, subres) ->
            go target prePieces subres
  where
   go curTarget prePieces subres = do
    -- The parent's static depth and how many dynamic pieces it consumes (only
    -- the count matters below, for the parent-dynamics pattern and the
    -- no-dynamics UrlToDispatch case).
    let parentDepth = length prePieces
        preDyns = [() | Dynamic _ <- prePieces]
        targetT = applyTyArgs (ConT (mkName curTarget)) tyargs

    -- UrlToDispatch/RedirectUrl is a top-level-only convenience and only
    -- possible when ParentArgs ~ () (no dynamic parent pieces).
    urlToDispatchInstances <- case (mmaster, preDyns) of
        (Just master, []) ->
            mkUrlToDispatchRedirectInstances cxt curTarget targetT master
        _ ->
            pure []

    -- Generate the parent dynamic argument variables once, and derive the
    -- binding pattern from them. The clause-generator is handed the @['Name']@
    -- directly (it used to re-decode them out of the pattern).
    parentDynVars <- forM preDyns $ \_ -> newName "parentDyn"
    parentDynsP <-
        case parentDynVars of
            []  -> [p| () |]
            [n] -> pure $ VarP n
            ns  -> pure $ TupP $ map VarP ns

    -- Generate names for the instance parameters
    toParentN <- newName "toParentRoute"
    envN <- newName "env"
    reqN <- newName "req"

    -- Generate dispatch clauses for each child resource
    clauses <- genNestedDispatchClauses
        config
        routeOpts
        parentDepth
        parentDynVars
        (VarE toParentN)
        (VarE envN)
        (VarE reqN)
        unwrapper
        tyargs
        subres

    let dispatchNestedT = ConT (ndcDispatchClass config) `AppT` targetT
        pathInfoExp = VarE 'W.pathInfo `AppE` VarE reqN
        dropExp = VarE 'drop `AppE` LitE (IntegerL $ fromIntegral parentDepth) `AppE` pathInfoExp
        thisDispatch = FunD (ndcDispatchFn config)
            [Clause
                [WildP, parentDynsP, VarP toParentN, VarP envN, VarP reqN]
                (NormalB $ CaseE dropExp clauses)
                []
            ]

    childInstances <-
        fmap mconcat $ forM subres $ \childRes -> do
            case childRes of
                ResourceParent name _ _ childPieces grandchildren -> do
                    rc <- resolveRouteCon name
                    instanceExists <- nestedInstanceExists (ndcDispatchClass config) rc
                    if instanceExists
                        then pure []
                        else do
                            -- Run the same arity guard the top-level
                            -- 'mkYesodSubDispatchInstance' applies, so a
                            -- 2nd-level-or-deeper nested datatype whose
                            -- parameter count doesn't match the (sub)site's
                            -- type arguments fails with the actionable message
                            -- rather than a cryptic kind error from generated
                            -- code. A no-op for the monomorphic (0-arity) case.
                            assertNestedSubArity
                                (maybe SubsiteCall (const TopLevelCall) mmaster)
                                (SubsiteName curTarget)
                                (SubsiteArity (tyArgsArity tyargs))
                                rc
                            -- Recurse on the already-resolved subtree: this
                            -- child's accumulated prefix is the parent's plus
                            -- this child's own pieces.
                            go name (prePieces <> childPieces) grandchildren
                _ -> pure []

    return
        ( instanceD cxt dispatchNestedT
            [ thisDispatch
            ]
        : childInstances <> urlToDispatchInstances
        )

-- | The top-level-only @UrlToDispatch@ + @RedirectUrl@ instances for a nested
-- route fragment whose @ParentArgs ~ ()@ (no dynamic parent pieces), letting
-- the fragment's constructor be used directly in @setUrl@\/@redirect@ without
-- the @WithParentArgs@ wrapper. Subsite nested dispatch emits none of these.
mkUrlToDispatchRedirectInstances
    :: Cxt
    -> String  -- ^ target route name (for the in-scope ToParentRoute check)
    -> Type    -- ^ the applied target route type
    -> Type    -- ^ the master site type
    -> Q [Dec]
mkUrlToDispatchRedirectInstances cxt target targetT master = do
    urlToDispatchT <- [t| UrlToDispatch $(pure targetT) $(pure master) |]
    urlToDispatchFn <- [e| toWaiAppYreNested (Proxy :: Proxy $(pure targetT)) () |]
    mYesodConstraint <- do
        hasYesodInstance <- isInstance ''Yesod [master]
        if hasYesodInstance
            then pure []
            else do
                yesodContext <- [t| Yesod $(pure master) |]
                pure [yesodContext]
    mToParentRouteConstraint <- do
        mtypeName <- lookupTypeName target
        parentRouteCxt <- [t| ToParentRoute $(pure targetT) |]
        case mtypeName of
            Nothing -> do
                -- must be generating it still. assume we don't
                -- have the instance in scope.
                pure [parentRouteCxt]
            Just _ -> do
                -- type is around, let's make sure it's not
                -- redundant?
                hasToParentRouteInstance <- isInstance ''ToParentRoute [targetT]
                if hasToParentRouteInstance
                    then pure []
                    else do
                        pure [parentRouteCxt]
    redirectT <- [t| RedirectUrl $(pure master) $(pure targetT) |]
    redirectUrlFn <- [e| toTextUrl . WithParentArgs () |]
    pure
        [ instanceD (cxt <> mYesodConstraint <> mToParentRouteConstraint) urlToDispatchT
            [ FunD 'urlToDispatch
                [ Clause [ WildP ] (NormalB urlToDispatchFn) []
                ]
            ]
        -- OVERLAPPABLE so a hand-written `RedirectUrl Site Child` instance for
        -- the same child route wins instead of clashing with this generated
        -- convenience instance.
        , InstanceD (Just Overlappable) (cxt <> mToParentRouteConstraint) redirectT
            [ FunD 'toTextUrl
                [ Clause [ ] (NormalB redirectUrlFn) [] ]
            ]
        ]

-- | Generate a 'YesodSubDispatchNested' instance for a nested route within
-- a subsite. Parallel to 'mkNestedDispatchInstance' but for the subsite case.
--
-- @since 1.7.0.0
mkNestedSubDispatchInstance
    :: RouteOpts
    -> String       -- ^ target nested route name
    -> Cxt          -- ^ instance context
    -> TyArgs -- ^ type arguments
    -> (Exp -> Q Exp) -- ^ unwrapper
    -> [ResourceTree Type] -- ^ all resources
    -> Q [Dec]
mkNestedSubDispatchInstance routeOpts target cxt tyargs unwrapper res =
    mkNestedDispatchInstanceWith subsiteNestedConfig Nothing
        routeOpts target cxt tyargs unwrapper res

-- | Generate dispatch clauses for nested dispatch instances.
-- Parameterized by 'NestedDispatchConfig' to support both
-- 'YesodDispatchNested' (top-level) and 'YesodSubDispatchNested' (subsite).
genNestedDispatchClauses
    :: NestedDispatchConfig
    -> RouteOpts
    -> Int -- ^ parent piece count (for error messages/debugging)
    -> [Name] -- ^ parent dynamic arg variables
    -> Exp -- ^ toParentRoute expression
    -> Exp -- ^ yre expression (YesodRunnerEnv or YesodSubRunnerEnv)
    -> Exp -- ^ req expression
    -> (Exp -> Q Exp) -- ^ unwrapper
    -> TyArgs -- ^ type arguments for parameterized routes
    -> [ResourceTree Type]
    -> Q [Match]
genNestedDispatchClauses config routeOpts _parentDepth parentDynVars toParentE yreE reqE unwrapper tyargs resources = do
    resourceClauses <- forM resources $ \res -> genClauseForResource res

    -- Terminal-miss clause: a nested dispatch instance ALWAYS returns 'Nothing'
    -- when no clause matches the remaining path, regardless of this module's
    -- fallthrough flag. 'yesodDispatchNested'/'yesodSubDispatchNested' are
    -- documented to return 'Nothing' on a miss so that the *caller* (the
    -- delegating parent clause, or the top-level/subsite terminal authority)
    -- decides whether the miss commits to a 404 or falls through to a sibling.
    -- Baking a 404 in here when fallthrough is disabled would let a split-out
    -- child silently override a fallthrough-wanting parent compiled in another
    -- module. The commit semantics of @fallthrough = False@ instead live at the
    -- delegating clauses (the 'ResourceParent' arm below and the inline parent
    -- body in 'mkDispatchClause') and at the terminal authorities.
    let fallbackClause = Match WildP (NormalB (ConE 'Nothing)) []

    return $ concat resourceClauses ++ [fallbackClause]
  where
    genClauseForResource :: ResourceTree Type -> Q [Match]
    genClauseForResource (ResourceLeaf (Resource name pieces dispatch _ _check)) = do
        (pats, dynVars) <- handlePiecesNames newName pieces
        let routeCon = applyConPieces name (map VarE dynVars)
            allDynVars = parentDynVars ++ dynVars

        case dispatch of
            Methods mmulti methods -> do
                -- Mirror the inline path (see 'handleDispatch' in
                -- 'mkDispatchClause'): a trailing multipiece binds a fresh
                -- @multi@ via 'EndMulti', and that value must be appended both
                -- to the route constructor's arguments and to the handler's
                -- arguments. Hardcoding 'EndExact' here would 404 any non-empty
                -- tail and build the constructor one argument short.
                (finalPat, mMultiE) <- case mmulti of
                    Nothing -> pure (EndExact, Nothing)
                    Just _ -> do
                        multiName <- newName "multi"
                        pure (EndMulti multiName, Just (VarE multiName))
                let dynExpsMulti = case mMultiE of
                        Nothing -> map VarE dynVars
                        Just e  -> map VarE dynVars ++ [e]
                    routeExp = toParentE `AppE` applyConPieces name dynExpsMulti
                    allDynExps = map VarE parentDynVars ++ dynExpsMulti
                handlerExp <- genHandlerCase name methods allDynExps
                let body = ConE 'Just `AppE`
                        (VarE (ndcRunnerFn config)
                            `AppE` handlerExp
                            `AppE` yreE
                            `AppE` (ConE 'Just `AppE` routeExp)
                            `AppE` reqE)
                return [Match (mkPathPat finalPat pats) (NormalB body) []]

            Subsite _ getSub -> do
                restPath <- newName "restPath"
                sub2 <- mkLambda "sub" $ \sub ->
                    pure $ foldl' (\a b -> a `AppE` b) (VarE (mkName getSub) `AppE` VarE sub) (map VarE allDynVars)
                routeLam <- mkLambda "sroute" $ \srouteN ->
                    pure $ toParentE `AppE` (routeCon `AppE` VarE srouteN)
                reqExp' <- do
                    setPath <- mkLambda "p" $ \pN -> mkLambda "r" $ \rN ->
                        pure $ RecUpdE (VarE rN) [('W.pathInfo, VarE pN)]
                    pure $ setPath `AppE` VarE restPath `AppE` reqE
                let subsiteExp = case ndcSubsiteEnv config of
                        DirectEnv ->
                            -- Top-level: construct YesodSubRunnerEnv directly
                            VarE 'yesodSubDispatch
                                `AppE` (RecConE 'YesodSubRunnerEnv
                                    [ ('ysreParentRunner, VarE 'yesodRunner)
                                    , ('ysreGetSub, sub2)
                                    , ('ysreToParentRoute, routeLam)
                                    , ('ysreParentEnv, yreE)
                                    ])
                                `AppE` reqExp'
                        ComposedEnv ->
                            -- Subsite: compose through the outer YesodSubRunnerEnv
                            let composeE f g = InfixE (Just f) (VarE '(.)) (Just g)
                            in VarE 'yesodSubDispatch
                                `AppE` (RecConE 'YesodSubRunnerEnv
                                    [ ('ysreParentRunner, VarE 'ysreParentRunner `AppE` yreE)
                                    , ('ysreGetSub, composeE sub2 (VarE 'ysreGetSub `AppE` yreE))
                                    , ('ysreToParentRoute, composeE (VarE 'ysreToParentRoute `AppE` yreE) routeLam)
                                    , ('ysreParentEnv, VarE 'ysreParentEnv `AppE` yreE)
                                    ])
                                `AppE` reqExp'
                return [Match (mkPathPat (EndRest restPath) pats) (NormalB $ ConE 'Just `AppE` subsiteExp) []]

    genClauseForResource (ResourceParent name _check _attrs pieces _children) = do
        (pats, dynVars) <- handlePiecesNames newName pieces

        -- Build the parent args tuple for the nested call
        let allDynVars = parentDynVars ++ dynVars
            parentArgsExp = parentArgsExpr allDynVars

            routeConWrapper = applyConPieces name (map VarE dynVars)
            toParentComposed = InfixE (Just toParentE) (VarE '(.)) (Just routeConWrapper)

        -- Apply type arguments to the route type for parameterized routes
        -- When tyargs is empty (e.g. from mkYesodSubDispatch), look up the type's arity
        routeType <- appliedRouteTypeNamed name tyargs
        let proxyExp = SigE (ConE 'Proxy) (AppT (ConT ''Proxy) routeType)

        resultName <- newName "k"

        let nestedCall = VarE (ndcDispatchFn config)
                `AppE` proxyExp
                `AppE` parentArgsExp
                `AppE` toParentComposed
                `AppE` yreE
                `AppE` reqE

        if roNestedRouteFallthrough routeOpts
            then do
                -- Fallthrough enabled: pattern-guard on a 'Just' result, so a
                -- 'Nothing' from the delegated child lets dispatch fall through
                -- to sibling clauses (and ultimately the 'Nothing' fallback).
                let patGuard = BindS (conPCompat 'Just [VarP resultName]) nestedCall
                    guardedBody = GuardedB [(PatG [patGuard], ConE 'Just `AppE` VarE resultName)]
                return [Match (mkPathPat EndWild pats) guardedBody []]
            else do
                -- Fallthrough disabled: once this parent's path prefix matches we
                -- commit to its subtree. If the delegated child dispatch returns
                -- 'Nothing', convert it to a 404 handler call rather than letting
                -- the 'Nothing' propagate (which, under an outer fallthrough
                -- caller, would let it fall through to siblings). This mirrors the
                -- inline no-fallthrough path's @Nothing -> Just 404@ shape.
                let notFoundExp =
                        VarE (ndcRunnerFn config)
                            `AppE` AppE (VarE 'void) (VarE 'notFound)
                            `AppE` yreE
                            `AppE` ConE 'Nothing
                            `AppE` reqE
                    committedBody =
                        ConE 'Just `AppE` (VarE 'fromMaybe `AppE` notFoundExp `AppE` nestedCall)
                return [Match (mkPathPat EndWild pats) (NormalB committedBody) []]

    genHandlerCase :: String -> [String] -> [Exp] -> Q Exp
    genHandlerCase name methods allDynExps = do
        let getHandlerName mmethod =
                let prefix = case mmethod of
                        Nothing -> "handle"
                        Just m -> map toLower m
                in mkName (prefix ++ name)

        if null methods
            then do
                -- No specific methods, just call handler
                let handlerName = getHandlerName Nothing
                handlerE <- unwrapper $ foldl' AppE (VarE handlerName) allDynExps
                let wrappedHandler = VarE 'fmap `AppE` VarE 'toTypedContent `AppE` handlerE
                return wrappedHandler
            else do
                -- Generate method case
                -- Wrap each handler with fmap toTypedContent so all branches have the same type
                methodMatches <- forM methods $ \method -> do
                    let handlerName = getHandlerName (Just method)
                    handlerE <- unwrapper $ foldl' AppE (VarE handlerName) allDynExps
                    let wrappedHandler = VarE 'fmap `AppE` VarE 'toTypedContent `AppE` handlerE
                    return $ Match (LitP $ StringL method) (NormalB wrappedHandler) []

                badMethodHandler <- unwrapper (VarE 'badMethod)
                let badMethodMatch = Match WildP (NormalB badMethodHandler) []
                    methodCase = CaseE (VarE 'W.requestMethod `AppE` reqE) (methodMatches ++ [badMethodMatch])

                return methodCase

mkYesodSubDispatch :: [ResourceTree a] -> Q Exp
mkYesodSubDispatch res = do
    let mds = (mkMDS
                return
                [|subHelper|]
                [|subTopDispatch|])
                { mdsNestedDispatchClass = ''YesodSubDispatchNested
                , mdsNestedDispatchFn = 'yesodSubDispatchNested
                }
    (_childNames, clause') <-
        mkDispatchClause
            []
            []
            NoTyArgs
            mds
            res
    inner <- newName "inner"
    let innerFun = FunD inner [clause']
    helper <- newName "helper"
    let fun = FunD helper
                [ Clause
                    []
                    (NormalB $ VarE inner)
                    [innerFun]
                ]
    return $ LetE [fun] (VarE helper)


subTopDispatch ::
    (YesodSubDispatch sub master) =>
        (forall content. ToTypedContent content =>
            SubHandlerFor child master content ->
            YesodSubRunnerEnv child master ->
            Maybe (Route child) ->
            W.Application
        ) ->
        (mid -> sub) ->
        (Route sub -> Route mid) ->
        YesodSubRunnerEnv mid master ->
        W.Application
subTopDispatch _ getSub toParent env = yesodSubDispatch
            (YesodSubRunnerEnv
            { ysreParentRunner = ysreParentRunner env
            , ysreGetSub = getSub . ysreGetSub env
            , ysreToParentRoute = ysreToParentRoute env . toParent
            , ysreParentEnv = ysreParentEnv env
            })

mkMDS :: (Exp -> Q Exp) -> Q Exp -> Q Exp -> MkDispatchSettings a site b
mkMDS unwrapper runHandlerE subDispatcher = MkDispatchSettings
    { mdsRunHandler = runHandlerE
    , mdsSubDispatcher = subDispatcher
    , mdsGetPathInfo = [|W.pathInfo|]
    , mdsSetPathInfo = [|\p r -> r { W.pathInfo = p }|]
    , mdsMethod = [|W.requestMethod|]
    , mds404 = [|void notFound|]
    , mds405 = [|void badMethod|]
    , mdsGetHandler = defaultGetHandler
    , mdsUnwrapper = unwrapper
    , mdsNestedRouteFallthrough = False
    , mdsNestedDispatchClass = ''YesodDispatchNested
    , mdsNestedDispatchFn = 'yesodDispatchNested
    }

parseYesodName :: String -> Either String (String, [String], [[String]])
parseYesodName name = do
    either (Left . show) Right $ parse parseName "" name
    where
        parseName = do
            cxt <- option [] parseContext
            name' <- parseWord
            args <- many parseWord
            spaces
            eof
            return ( name', args, cxt)

        parseWord = do
            spaces
            many1 alphaNum

        parseContext = try $ do
            cxts <- parseParen parseContexts
            spaces
            _ <- string "=>"
            return cxts

        parseParen p = do
            spaces
            _ <- char '('
            r <- p
            spaces
            _ <- char ')'
            return r

        parseContexts =
            sepBy1 (many1 parseWord) (spaces >> char ',' >> return ())
