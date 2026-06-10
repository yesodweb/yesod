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
    , NestedTarget (..)
    , SameSpliceNestedInstances (..)
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
    , mdsNestedTarget :: NestedTarget
    -- ^ Whether this dispatch is for a top-level site or a subsite. Selects the
    -- nested-dispatch class to probe ('nestedTargetClass') and the
    -- nested-dispatch function to delegate to ('nestedTargetFn'):
    -- 'TopLevelNested' uses @YesodDispatchNested@\/@yesodDispatchNested@,
    -- 'SubsiteNested' uses @YesodSubDispatchNested@\/@yesodSubDispatchNested@.
    --
    -- @since 1.7.0.0
    , mdsSameSpliceNestedInstances :: !SameSpliceNestedInstances
    -- ^ Whether the caller emits the matching per-parent nested-dispatch
    -- instances in the same splice as this flat dispatch body; see
    -- 'SameSpliceNestedInstances' for how this affects a parent clause.
    -- 'mkMDS' defaults to 'NoSameSpliceNestedInstances'.
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

-- | Whether nested-route dispatch is being generated for a top-level site or
-- for a subsite. This is the single bit that selects the whole top-vs-subsite
-- plumbing: the dispatch class to probe and the function to delegate to (used
-- by 'MkDispatchSettings' via 'mdsNestedTarget'), and the runner function,
-- dispatch function\/class, and subsite-environment construction used when
-- generating nested-dispatch instances. All of those follow from this enum via
-- the @nestedTarget*@ derivation functions below.
--
-- @since 1.7.0.0
data NestedTarget
    = TopLevelNested
    -- ^ A top-level site: @YesodDispatchNested@\/@yesodDispatchNested@, run via
    -- @yesodRunner@, constructing 'YesodSubRunnerEnv' directly.
    | SubsiteNested
    -- ^ A subsite: @YesodSubDispatchNested@\/@yesodSubDispatchNested@, run via
    -- @subHelper@, composing through the outer 'YesodSubRunnerEnv' (like
    -- 'subTopDispatch').
    deriving (Eq, Show)

-- | Whether the splice emitting a flat dispatch body also generates the
-- matching per-parent nested-dispatch instances (@YesodDispatchNested@ \/
-- @YesodSubDispatchNested@) in the same declaration group.
--
-- @since 1.7.0.0
data SameSpliceNestedInstances
    = GeneratesNestedInstances
    -- ^ The caller emits the per-parent nested instances alongside the flat
    -- dispatch, so a parent clause /delegates/ to its instance even though the
    -- instance does not exist at probe time — GHC resolves it once the whole
    -- declaration group is spliced. This avoids inlining each nested leaf's
    -- dispatch into the flat body /and/ re-emitting it in the per-parent
    -- instance (see 'mkTopLevelDispatchInstance').
    | NoSameSpliceNestedInstances
    -- ^ Only the flat dispatch is emitted. A parent delegates only when its
    -- nested instance already exists (the cross-module split case) and inlines
    -- otherwise — required for standalone entry points like
    -- 'mkYesodSubDispatch', where delegating would reference a missing
    -- instance.
    deriving (Eq, Show)

-- | The nested-dispatch class to probe\/emit for a target:
-- @''YesodDispatchNested@ for top-level, @''YesodSubDispatchNested@ for subsites.
nestedTargetClass :: NestedTarget -> Name
nestedTargetClass TopLevelNested = ''YesodDispatchNested
nestedTargetClass SubsiteNested  = ''YesodSubDispatchNested

-- | The nested-dispatch function to delegate to\/emit for a target:
-- @'yesodDispatchNested@ for top-level, @'yesodSubDispatchNested@ for subsites.
nestedTargetFn :: NestedTarget -> Name
nestedTargetFn TopLevelNested = 'yesodDispatchNested
nestedTargetFn SubsiteNested  = 'yesodSubDispatchNested

-- | The runner function used inside generated nested-dispatch clauses:
-- @'yesodRunner@ for top-level, @'subHelper@ for subsites.
nestedTargetRunner :: NestedTarget -> Name
nestedTargetRunner TopLevelNested = 'yesodRunner
nestedTargetRunner SubsiteNested  = 'subHelper

-- | The 'ArityCallSite' for arity-mismatch error messages: a top-level site is
-- reached through @mkYesod@, a subsite through @mkYesodSubDispatchInstance@.
nestedTargetCallSite :: NestedTarget -> ArityCallSite
nestedTargetCallSite TopLevelNested = TopLevelCall
nestedTargetCallSite SubsiteNested  = SubsiteCall

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
            liftQ (nestedInstanceExists (nestedTargetClass mdsNestedTarget) =<< resolveRouteCon name)

        -- Delegate to the nested-dispatch instance either when one already
        -- exists (cross-module split) or when the caller will generate it in
        -- this same splice ('mdsSameSpliceNestedInstances'). In the latter case
        -- the instance does not exist at probe time, but GHC resolves it after
        -- the whole declaration group is spliced — so we avoid inlining every
        -- nested leaf's dispatch logic here only to re-emit it in the parent's
        -- 'YesodDispatchNested' instance. We still report the name below so that
        -- instance actually gets generated.
        let delegate =
                instanceExists || mdsSameSpliceNestedInstances == GeneratesNestedInstances

        (pats, dyns) <- liftQ $ handlePiecesM pieces
        restName <- liftQ $ newName "_rest"
        helperName <- liftQ $ newName ("helper" ++ name)
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
        -- a yet-to-be-generated same-splice instance
        -- ('mdsSameSpliceNestedInstances').
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
        (pats, dyns) <- liftQ $ handlePiecesM pieces
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
        liftQ (nestedDispatchCall (nestedTargetFn mdsNestedTarget) name dyns tyargs sdc thisRouteParentArgs)

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
                    baseNotFoundExp <-
                        [| $(mdsRunHandler) $(mds404) $(pure (envExp sdc)) Nothing $(pure (reqExp sdc)) |]
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
                    let allDyns = extraParams ++ dyns
                    sub2 <- mkLambda "sub" $ \sub ->
                        pure $ foldl' (\a b -> a `AppE` b) (VarE (mkName getSub) `AppE` VarE sub) allDyns
                    route <- mkLambda "sroute" $ \sroute ->
                        pure $ let route' = applyConPieces name dyns
                               in foldr AppE (AppE route' $ VarE sroute) extraCons
                    exp <-
                        [| $(mdsSubDispatcher)
                            $(mdsRunHandler)
                            $(pure sub2)
                            $(pure route)
                            $(pure envExp)
                            ($(mdsSetPathInfo) $(varE restPath) $(pure reqExp)) |]
                    return (exp, EndRest restPath)

    -- The dispatch helper produced by 'mkDispatchClause' is always a terminal
    -- authority (the top-level / subsite entry point), so a final miss commits
    -- to a 404 here. Inline nested helpers built by 'go' under 'withChildScope'
    -- bake their own @Nothing@ fallback at their call site instead.
    mkClause404 envE reqE = do
        exp <- [| $(mdsRunHandler) $(mds404) $(pure envE) Nothing $(pure reqE) |]
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
    routeType <- appliedRouteTypeNamed routeName tyargs
    let wrapper = foldr composeE (applyConPieces routeName routeDyns) (extraCons sdc)
    [| $(varE dispatchFn)
        (Proxy :: Proxy $(pure routeType))
        $(pure $ parentArgsExprFromExps parentDyns)
        $(pure wrapper)
        $(pure $ envExp sdc)
        $(pure $ reqExp sdc)
     |]

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
            , mdsSameSpliceNestedInstances =
                if usesNestedDiscovery
                    then GeneratesNestedInstances
                    else NoSameSpliceNestedInstances
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
    mkNestedDispatchInstanceWith TopLevelNested (Just master)
        routeOpts target cxt tyargs unwrapper res

-- | The shared body of the top-level ('mkNestedDispatchInstance') and subsite
-- ('mkNestedSubDispatchInstance') nested-dispatch instance generators. Both
-- find the target, build the parent-dynamics pattern, generate the dispatch
-- clauses via 'genNestedDispatchClauses', emit one
-- 'nestedTargetClass'\/'nestedTargetFn' instance, and recurse into nested
-- children. They differ only in the 'NestedTarget' (which selects the
-- dispatch function\/class, runner, and subsite-env construction) and whether a
-- master site is in play: a @'Just' master@ marks the top-level case and additionally emits
-- the @UrlToDispatch@\/@RedirectUrl@ instances, which the subsite case
-- ('Nothing') never produces.
mkNestedDispatchInstanceWith
    :: NestedTarget
    -> Maybe Type            -- ^ @Just master@ ⇒ top-level (also emit UrlToDispatch\/RedirectUrl); @Nothing@ ⇒ subsite
    -> RouteOpts
    -> String
    -> Cxt
    -> TyArgs
    -> (Exp -> Q Exp)
    -> [ResourceTree Type]
    -> Q [Dec]
mkNestedDispatchInstanceWith nestedTarget mmaster routeOpts target cxt tyargs unwrapper res = do
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
        nestedTarget
        routeOpts
        parentDynVars
        (VarE toParentN)
        (VarE envN)
        (VarE reqN)
        unwrapper
        tyargs
        subres

    dropExp <- [| drop parentDepth (W.pathInfo $(varE reqN)) |]
    let dispatchNestedT = ConT (nestedTargetClass nestedTarget) `AppT` targetT
        thisDispatch = FunD (nestedTargetFn nestedTarget)
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
                    instanceExists <- nestedInstanceExists (nestedTargetClass nestedTarget) rc
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
                                (nestedTargetCallSite nestedTarget)
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
    mkNestedDispatchInstanceWith SubsiteNested Nothing
        routeOpts target cxt tyargs unwrapper res

-- | Generate dispatch clauses for nested dispatch instances.
-- Parameterized by 'NestedTarget' to support both
-- 'YesodDispatchNested' (top-level) and 'YesodSubDispatchNested' (subsite).
genNestedDispatchClauses
    :: NestedTarget
    -> RouteOpts
    -> [Name] -- ^ parent dynamic arg variables
    -> Exp -- ^ toParentRoute expression
    -> Exp -- ^ yre expression (YesodRunnerEnv or YesodSubRunnerEnv)
    -> Exp -- ^ req expression
    -> (Exp -> Q Exp) -- ^ unwrapper
    -> TyArgs -- ^ type arguments for parameterized routes
    -> [ResourceTree Type]
    -> Q [Match]
genNestedDispatchClauses nestedTarget routeOpts parentDynVars toParentE yreE reqE unwrapper tyargs resources = do
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
                body <-
                    [| Just ($(varE (nestedTargetRunner nestedTarget))
                                $(pure handlerExp)
                                $(pure yreE)
                                (Just $(pure routeExp))
                                $(pure reqE)) |]
                return [Match (mkPathPat finalPat pats) (NormalB body) []]

            Subsite _ getSub -> do
                restPath <- newName "restPath"
                sub2 <- mkLambda "sub" $ \sub ->
                    pure $ foldl' (\a b -> a `AppE` b) (VarE (mkName getSub) `AppE` VarE sub) (map VarE allDynVars)
                routeLam <- mkLambda "sroute" $ \srouteN ->
                    pure $ toParentE `AppE` (routeCon `AppE` VarE srouteN)
                let reqExp' = RecUpdE reqE [('W.pathInfo, VarE restPath)]
                body <- case nestedTarget of
                    TopLevelNested ->
                        -- Top-level: construct YesodSubRunnerEnv directly
                        [| Just (yesodSubDispatch
                            YesodSubRunnerEnv
                                { ysreParentRunner = yesodRunner
                                , ysreGetSub = $(pure sub2)
                                , ysreToParentRoute = $(pure routeLam)
                                , ysreParentEnv = $(pure yreE)
                                }
                            $(pure reqExp')) |]
                    SubsiteNested ->
                        -- Subsite: compose through the outer YesodSubRunnerEnv
                        [| Just (yesodSubDispatch
                            YesodSubRunnerEnv
                                { ysreParentRunner = ysreParentRunner $(pure yreE)
                                , ysreGetSub = $(pure sub2) . ysreGetSub $(pure yreE)
                                , ysreToParentRoute = ysreToParentRoute $(pure yreE) . $(pure routeLam)
                                , ysreParentEnv = ysreParentEnv $(pure yreE)
                                }
                            $(pure reqExp')) |]
                return [Match (mkPathPat (EndRest restPath) pats) (NormalB body) []]

    genClauseForResource (ResourceParent name _check _attrs pieces _children) = do
        (pats, dynVars) <- handlePiecesNames pieces

        -- Build the parent args tuple for the nested call
        let allDynVars = parentDynVars ++ dynVars
            parentArgsExp = parentArgsExpr allDynVars
            routeConWrapper = applyConPieces name (map VarE dynVars)

        routeType <- appliedRouteTypeNamed name tyargs

        resultName <- newName "k"

        nestedCall <-
            [| $(varE (nestedTargetFn nestedTarget))
                (Proxy :: Proxy $(pure routeType))
                $(pure parentArgsExp)
                ($(pure toParentE) . $(pure routeConWrapper))
                $(pure yreE)
                $(pure reqE) |]

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
                committedBody <-
                    [| Just (fromMaybe
                        ($(varE (nestedTargetRunner nestedTarget))
                            (void notFound) $(pure yreE) Nothing $(pure reqE))
                        $(pure nestedCall)) |]
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
                [| fmap toTypedContent $(pure handlerE) |]
            else do
                -- Generate method case
                -- Wrap each handler with fmap toTypedContent so all branches have the same type
                methodMatches <- forM methods $ \method -> do
                    handlerE <- handlerExpFor (Just method)
                    wrappedHandler <- [| fmap toTypedContent $(pure handlerE) |]
                    return $ Match (LitP $ StringL method) (NormalB wrappedHandler) []

                badMethodHandler <- unwrapper (VarE 'badMethod)
                let badMethodMatch = Match WildP (NormalB badMethodHandler) []
                scrutinee <- [| W.requestMethod $(pure reqE) |]
                return $ CaseE scrutinee (methodMatches ++ [badMethodMatch])

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
mkYesodSubDispatchWith = mkYesodSubDispatchWithDelegate NoSameSpliceNestedInstances

-- | The body-generation core shared by the standalone 'mkYesodSubDispatchWith'
-- and the instance-generating 'mkYesodSubDispatchInstanceOpts'. The
-- 'SameSpliceNestedInstances' argument says whether the caller emits the
-- per-parent 'YesodSubDispatchNested' instances in the same splice — see that
-- type for what each choice means for the generated body.
--
-- @since 1.7.0.0
mkYesodSubDispatchWithDelegate
    :: SameSpliceNestedInstances -> RouteOpts -> [ResourceTree a] -> Q Exp
mkYesodSubDispatchWithDelegate sameSplice routeOpts res = do
    let mds = (mkMDS
                return
                [|subHelper|]
                [|subTopDispatch|])
                { mdsNestedTarget = SubsiteNested
                , mdsNestedRouteFallthrough = roNestedRouteFallthrough routeOpts
                , mdsSameSpliceNestedInstances = sameSplice
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
    , mdsNestedTarget = TopLevelNested
    , mdsSameSpliceNestedInstances = NoSameSpliceNestedInstances
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
