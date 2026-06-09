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
    , mkYesodSubDispatchWith
    , mkYesodSubDispatchWithDelegate
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
import Control.Monad.Reader (ReaderT, runReaderT, asks, local, lift)
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
    , mdsNestedDelegateInline :: !Bool
    -- ^ When 'True', a parent's dispatch clause /delegates/ to its
    -- nested-dispatch instance even when no such instance exists /yet/ — because
    -- the caller will generate it in the same splice (see
    -- 'mkTopLevelDispatchInstance'). This avoids inlining each nested leaf's
    -- dispatch logic into the flat @yesodDispatch@ /and/ re-emitting it in the
    -- per-parent @YesodDispatchNested@ instance: GHC resolves the same-splice
    -- instance after the whole declaration group is spliced, so the flat clause
    -- can call @yesodDispatchNested@ for an instance declared alongside it.
    --
    -- When 'False' (the default), a parent only delegates if its instance
    -- already exists (the cross-module split case) and otherwise inlines, which
    -- is required for callers that emit the flat dispatch /without/ generating
    -- the matching nested instances in the same splice (e.g. a bare
    -- 'mkYesodSubDispatch').
    --
    -- @since 1.7.0.0
    }

data SDC = SDC
    { extraParams :: [Exp]
    , extraCons :: [Exp]
    , envExp :: Exp
    , reqExp :: Exp
    }

-- | The reader environment threaded through 'mkDispatchClause's clause
-- generator. 'envSdc' is the accumulated dispatch context (extended via 'local'
-- as we descend into a parent's children); 'envWrap' is how a matched result is
-- wrapped — 'id' for the top-level (unwrapped) dispatch result, and 'Just' for
-- the inline nested helper clauses, where the enclosing parent needs to tell a
-- match from a fall-through miss. The top-level call starts at 'id' and flips
-- to 'Just' on the first descent (and never back), so the two phases are a
-- single 'local' rather than two functions.
data Env = Env
    { envSdc :: SDC
    , envWrap :: Exp -> Exp
    }

-- | The monad 'mkDispatchClause's clause generator runs in: 'Q' carrying an
-- 'Env' read through 'MonadReader'. Every helper that needs the dispatch
-- context or the result wrapper takes them from the environment ('asks' /
-- 'local') rather than as arguments; 'liftQ' lifts the concrete-'Q' primitives.
type DispatchM = ReaderT Env Q

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
-- The function returns the dispatch 'Clause' along with a @['String']@ of the
-- names of nested route types that require a delegation instance to be
-- generated. For 'YesodDispatch', those are 'YesodDispatchNested' instances; for
-- the subsite case, 'YesodSubDispatchNested'.
--
-- @since 1.7.0.0 — the leading @[Name]@\/@[Exp]@ parameters were dropped and the
-- result type changed from @Clause@ to @Q ([String], Clause)@.
mkDispatchClause :: forall a b site c. TyArgs -> MkDispatchSettings b site c -> [ResourceTree a] -> Q ([String], Clause)
mkDispatchClause tyargs MkDispatchSettings {..} resources = do
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
            { extraParams = []
            , extraCons = []
            , envExp = envE
            , reqExp = reqE
            }
    -- Generate the dispatch clauses. 'go' runs in @'ReaderT' 'Env' Q@: the
    -- top-level call starts with @envWrap = id@ so the top-level resources
    -- produce the final (unwrapped) dispatch result and report which parents
    -- need a nested-dispatch instance generated. Descending into a parent's
    -- inline children flips @envWrap@ to 'Just' (via 'local'), so children are
    -- 'Just'-wrapped helper clauses the enclosing parent can fall through on;
    -- their reported names are dropped (only top-level parents matter).
    let topEnv = Env { envSdc = sdc, envWrap = id }
    (childNames, clauses) <- mconcat <$> runReaderT (mapM go resources) topEnv

    pure
        ( childNames
        , Clause
            [VarP envName, VarP reqName]
            (NormalB $ helperE `AppE` pathInfo)
            [FunD helperName $ clauses ++ [clause404']]
        )
  where
    -- Lift a concrete-'Q' action (the TH primitives and this package's own
    -- @Q@-typed helpers) into 'DispatchM'.
    liftQ :: Q x -> DispatchM x
    liftQ = lift

    -- Apply the current phase's result wrapper, read from the environment: 'id'
    -- at the top level (unwrapped dispatch result) and 'Just' once inside a
    -- parent's inline children, where the parent must tell a match from a
    -- fall-through miss.
    wrapResult :: Exp -> DispatchM Exp
    wrapResult e = asks (($ e) . envWrap)

    -- Wrap a result in 'Just' — the nested-phase 'envWrap'.
    justE :: Exp -> Exp
    justE e = ConE 'Just `AppE` e

    -- Run an action in the scope of a parent's inline children: extend the
    -- accumulated dynamics and parent constructors with this parent's, and flip
    -- 'envWrap' to 'Just'. This single 'local' — entered once at the top→nested
    -- boundary and never undone — is the entire top-vs-nested phase distinction.
    withChildScope :: [Exp] -> Exp -> DispatchM r -> DispatchM r
    withChildScope dyns constr = local $ \e ->
        let sdc = envSdc e
        in e { envSdc = sdc
                 { extraParams = extraParams sdc ++ dyns
                 , extraCons = extraCons sdc ++ [constr]
                 }
             , envWrap = justE
             }

    -- | Generate the dispatch clauses for a resource tree node, plus the
    -- @['String']@ of parents that need a nested-dispatch instance generated.
    -- Names are reported for every parent here, but only the top-level call
    -- keeps them; the parent arm drops its children's names (the deeper
    -- instances are generated by a separate recursion in
    -- 'mkNestedDispatchInstanceWith').
    go :: ResourceTree a -> DispatchM ([String], [Clause])
    go (ResourceParent name _check _attrs pieces children) = do
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
            liftQ (nestedInstanceExists mdsNestedDispatchClass =<< resolveRouteCon name)

        -- Delegate to the nested-dispatch instance either when one already
        -- exists (cross-module split) or when the caller will generate it in
        -- this same splice ('mdsNestedDelegateInline'). In the latter case the
        -- instance does not exist at probe time, but GHC resolves it after the
        -- whole declaration group is spliced — so we avoid inlining every nested
        -- leaf's dispatch logic here only to re-emit it in the parent's
        -- 'YesodDispatchNested' instance. We still report the name below so that
        -- instance actually gets generated.
        let delegate = instanceExists || mdsNestedDelegateInline

        (pats, dyns) <- handlePiecesM pieces
        restName <- freshName "_rest"
        helperName <- freshName ("helper" ++ name)
        let helperCall = VarE helperName `AppE` VarE restName
            constr = applyConPieces name dyns

        helperClauses <-
            if delegate
                then do
                    expr <- delegateToNestedInstance name dyns
                    pure [Clause [VarP restName] (NormalB expr) []]
                else do
                    -- Inline dispatch: descend into the children with the
                    -- extended scope and 'Just' wrapping; their reported names
                    -- are dropped (only top-level parents matter).
                    childClauses <-
                        concatMap snd <$> withChildScope dyns constr (mapM go children)
                    pure $ childClauses ++ [Clause [WildP] (NormalB (ConE 'Nothing)) []]

        body <- parentBody helperCall

        -- Report this parent as needing a nested-dispatch instance whenever one
        -- does not already exist — both when we inlined and when we delegated to
        -- a yet-to-be-generated same-splice instance ('mdsNestedDelegateInline').
        -- Whether the caller acts on this (i.e. actually generates the instance)
        -- is the caller's decision — see 'mkDispatchInstance'.
        pure
            ( [name | not instanceExists]
            , [ Clause
                [mkPathPat (EndRest restName) pats]
                body
                [FunD helperName helperClauses] ]
            )

    go (ResourceLeaf (Resource name pieces dispatch _ _check)) = do
        (pats, dyns) <- handlePiecesM pieces
        (chooseMethod, finalPat) <- handleDispatch name dispatch dyns
        clauseBody <- NormalB <$> wrapResult chooseMethod
        pure ([], [Clause [mkPathPat finalPat pats] clauseBody []])

    -- | Delegate body for a parent that already has a nested-dispatch instance:
    -- call the configured nested-dispatch function, passing this route's
    -- dynamics — plus the accumulated parent dynamics from the environment — as
    -- ParentArgs.
    delegateToNestedInstance :: String -> [Exp] -> DispatchM Exp
    delegateToNestedInstance name dyns = do
        sdc <- asks envSdc
        let thisRouteParentArgs = extraParams sdc ++ dyns
        liftQ (nestedDispatchCall mdsNestedDispatchFn name dyns tyargs sdc thisRouteParentArgs)

    -- | The body of a parent dispatch clause. @helperCall@ runs the parent's
    -- inline helper on the remaining path; per the fallthrough flag we either
    -- fall through on a 'Nothing' (pattern guard) or commit to a 404. The
    -- matched result is run through the current 'envWrap'.
    parentBody :: Exp -> DispatchM Body
    parentBody helperCall = do
        wrap <- asks envWrap
        if mdsNestedRouteFallthrough
            then liftQ $ mkGuardedBody helperCall (\match' -> pure (wrap (VarE match')))
            else do
                sdc <- asks envSdc
                liftQ $ do
                    matchName <- newName "match"
                    handler <- mds404
                    runHandlerE <- mdsRunHandler
                    let baseNotFoundExp = runHandlerE `AppE` handler `AppE` envExp sdc `AppE` ConE 'Nothing `AppE` reqExp sdc
                    pure $ NormalB $ CaseE helperCall
                        [ Match (conPCompat 'Just [VarP matchName]) (NormalB (wrap (VarE matchName))) []
                        , Match (conPCompat 'Nothing []) (NormalB (wrap baseNotFoundExp)) []
                        ]

    -- | Build the chosen-method expression and final path tail for a leaf,
    -- reading the dispatch context ('extraParams' / 'extraCons' / 'envExp' /
    -- 'reqExp') from the environment.
    handleDispatch :: String -> Dispatch a -> [Exp] -> DispatchM (Exp, PathTail)
    handleDispatch name dispatch' dyns = do
        SDC {..} <- asks envSdc
        liftQ $ case dispatch' of
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
    -- to a 404 here. Inline nested helpers built by 'go' under 'withChildScope'
    -- bake their own @Nothing@ fallback at their call site instead.
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
        -- Under nested discovery this instance also emits a
        -- 'YesodDispatchNested' instance per top-level parent (see
        -- 'childNamesToGenerate'). When it does, the flat @yesodDispatch@ clause
        -- can /delegate/ each parent to that same-splice instance rather than
        -- inlining the whole subtree's dispatch logic (which the nested instance
        -- would then re-emit). Under 'InlineCompat' no nested instances are
        -- generated, so the flat clause must inline as before.
        usesNestedDiscovery =
            case discoveryMode routeOpts (hasTyArgs tyargs) of
                NestedDiscovery -> True
                InlineCompat    -> False
        mdsWithNestedDispatch = mds
            { mdsNestedRouteFallthrough = roNestedRouteFallthrough routeOpts
            , mdsNestedDelegateInline = usesNestedDiscovery
            }
    (childNames, clause') <- mkDispatchClause tyargs mdsWithNestedDispatch res
    let thisDispatch = FunD 'yesodDispatch [clause']
        -- Only generate 'YesodDispatchNested' instances for children when this
        -- site uses nested discovery. A parameterized site that has not opted
        -- in keeps unparameterized subroute datatypes (see 'discoveryMode' in
        -- RenderRoute), so generating nested instances — which would reference
        -- the parameterized form — must be suppressed to stay consistent.
        childNamesToGenerate =
            if usesNestedDiscovery then childNames else []
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
--
-- @since 1.7.0.0
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
    let parentDynsP = parentArgsPat parentDynVars

    -- Generate names for the instance parameters
    toParentN <- newName "toParentRoute"
    envN <- newName "env"
    reqN <- newName "req"

    -- Generate dispatch clauses for each child resource
    clauses <- genNestedDispatchClauses
        config
        routeOpts
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
        -- OVERLAPPABLE so a strictly more specific hand-written instance (e.g.
        -- one carrying type variables, as for a parameterized site) takes
        -- precedence over this generated convenience instance. Note that a
        -- hand-written instance with an identical fully-concrete head still
        -- collides as a duplicate, regardless of this pragma.
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
    -> [Name] -- ^ parent dynamic arg variables
    -> Exp -- ^ toParentRoute expression
    -> Exp -- ^ yre expression (YesodRunnerEnv or YesodSubRunnerEnv)
    -> Exp -- ^ req expression
    -> (Exp -> Q Exp) -- ^ unwrapper
    -> TyArgs -- ^ type arguments for parameterized routes
    -> [ResourceTree Type]
    -> Q [Match]
genNestedDispatchClauses config routeOpts parentDynVars toParentE yreE reqE unwrapper tyargs resources = do
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
        (pats, dynVars) <- handlePiecesNames pieces
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
                let reqExp' = RecUpdE reqE [('W.pathInfo, VarE restPath)]
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
        (pats, dynVars) <- handlePiecesNames pieces

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
        let handlerExpFor mmethod = do
                handlerName <- defaultGetHandler mmethod name
                unwrapper $ foldl' AppE handlerName allDynExps

        if null methods
            then do
                -- No specific methods, just call handler
                handlerE <- handlerExpFor Nothing
                let wrappedHandler = VarE 'fmap `AppE` VarE 'toTypedContent `AppE` handlerE
                return wrappedHandler
            else do
                -- Generate method case
                -- Wrap each handler with fmap toTypedContent so all branches have the same type
                methodMatches <- forM methods $ \method -> do
                    handlerE <- handlerExpFor (Just method)
                    let wrappedHandler = VarE 'fmap `AppE` VarE 'toTypedContent `AppE` handlerE
                    return $ Match (LitP $ StringL method) (NormalB wrappedHandler) []

                badMethodHandler <- unwrapper (VarE 'badMethod)
                let badMethodMatch = Match WildP (NormalB badMethodHandler) []
                    methodCase = CaseE (VarE 'W.requestMethod `AppE` reqE) (methodMatches ++ [badMethodMatch])

                return methodCase

mkYesodSubDispatch :: [ResourceTree a] -> Q Exp
mkYesodSubDispatch = mkYesodSubDispatchWith defaultOpts

-- | Like 'mkYesodSubDispatch', but threads a 'RouteOpts' into the generated
-- @yesodSubDispatch@ body. The only option that affects the body is
-- 'roNestedRouteFallthrough', which controls whether a subsite's top-level
-- parent clause falls through to a later sibling on an inner miss (mirroring
-- 'mkTopLevelDispatchInstance'). 'mkYesodSubDispatch' keeps the opts-less
-- signature for backwards compatibility.
--
-- This standalone entry point never delegates a parent's dispatch to a
-- same-splice 'YesodSubDispatchNested' instance: when called on its own there
-- are no such instances being generated alongside it (delegating would emit a
-- reference to a missing instance). 'mkYesodSubDispatchInstanceOpts', which
-- /does/ generate the matching nested instances in the same splice, uses
-- 'mkYesodSubDispatchWithDelegate' to opt into delegation.
--
-- @since 1.7.0.0
mkYesodSubDispatchWith :: RouteOpts -> [ResourceTree a] -> Q Exp
mkYesodSubDispatchWith = mkYesodSubDispatchWithDelegate False

-- | The body-generation core shared by the standalone 'mkYesodSubDispatchWith'
-- and the instance-generating 'mkYesodSubDispatchInstanceOpts'. The 'Bool'
-- selects 'mdsNestedDelegateInline': 'False' inlines each parent's subtree
-- dispatch (correct for the standalone public API, where no same-splice nested
-- instances exist), 'True' delegates each parent to its 'YesodSubDispatchNested'
-- instance generated alongside it (avoiding the doubled codegen, mirroring
-- 'mkTopLevelDispatchInstance' on the top-level path).
--
-- @since 1.7.0.0
mkYesodSubDispatchWithDelegate :: Bool -> RouteOpts -> [ResourceTree a] -> Q Exp
mkYesodSubDispatchWithDelegate delegateInline routeOpts res = do
    let mds = (mkMDS
                return
                [|subHelper|]
                [|subTopDispatch|])
                { mdsNestedDispatchClass = ''YesodSubDispatchNested
                , mdsNestedDispatchFn = 'yesodSubDispatchNested
                , mdsNestedRouteFallthrough = roNestedRouteFallthrough routeOpts
                , mdsNestedDelegateInline = delegateInline
                }
    (_childNames, clause') <-
        mkDispatchClause
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
    , mdsNestedDelegateInline = False
    }

-- | Parse the foundation-type string given to @mkYesod@ into its components:
-- the type-constructor name, its type arguments, and the @=>@ class-context
-- groups (each a class name applied to type arguments). Returns 'Left' with a
-- parse-error message on malformed input.
--
-- @since 1.7.0.0
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
