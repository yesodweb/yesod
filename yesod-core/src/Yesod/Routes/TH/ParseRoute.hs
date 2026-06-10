{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Yesod.Routes.TH.ParseRoute
    ( -- ** ParseRoute
      mkParseRouteInstance
    , mkParseRouteInstanceOpts
    , mkParseRouteInstanceFor
      -- ** Clause construction (monad-polymorphic over 'Quote'; testable purely)
    , buildInlineParseClauses
    , generateParseRouteClause
    ) where

import qualified Control.Monad.Trans as Trans
import qualified Data.Set as Set
import Control.Monad
import Yesod.Routes.TH.Types
import Language.Haskell.TH (varE, varP)
import Language.Haskell.TH.Syntax hiding (newName)
import Language.Haskell.TH.Syntax.Compat (Quote(..), unsafeQToQuote)
import Yesod.Routes.Class
import Yesod.Routes.TH.RenderRoute
import Control.Monad.State.Strict
import Yesod.Routes.TH.Internal

mkParseRouteInstance :: TyArgs -> Cxt -> Type -> [ResourceTree a] -> Q [Dec]
mkParseRouteInstance =
    mkParseRouteInstanceOpts defaultOpts

-- | Generate a ParseRouteNested instance for a specific nested route target.
-- This is a convenience wrapper around mkParseRouteInstanceOpts with setFocusOnNestedRoute.
--
-- @since 1.7.0.0
mkParseRouteInstanceFor :: String -> [ResourceTree a] -> Q [Dec]
mkParseRouteInstanceFor target ress = do
    let opts = setFocusOnNestedRoute target defaultOpts
        targetType = ConT (mkName target)
    mkParseRouteInstanceOpts opts NoTyArgs [] targetType ress

-- | Generate the 'ParseRoute' instance (and, under nested discovery, the
-- accompanying 'ParseRouteNested' instances) for a site, honoring the supplied
-- 'RouteOpts' and the site's 'TyArgs'. The options-driven core that
-- 'mkParseRouteInstanceFor' and the @mkYesod@ entry points build on.
--
-- @since 1.7.0.0
mkParseRouteInstanceOpts :: RouteOpts -> TyArgs -> Cxt -> Type -> [ResourceTree a] -> Q [Dec]
mkParseRouteInstanceOpts routeOpts origTyargs cxt typ unfocusedRess =
    case discoveryMode routeOpts (hasTyArgs origTyargs) of
        -- Backwards-compatible default: generate a single 'ParseRoute'
        -- instance with all nested routes inlined, and no 'ParseRouteNested'
        -- instances.
        InlineCompat -> do
            clausess <- mapM (generateParseRouteClausesInline id) unfocusedRess
            let missingClause = Clause [WildP] (NormalB (ConE 'Nothing)) []
                allClauses = concat clausess <> [missingClause]
            pure
                [ instanceD cxt (ConT ''ParseRoute `AppT` typ)
                    [ FunD 'parseRoute allClauses
                    ]
                ]
        NestedDiscovery -> do
            ress <- focusTarget unfocusedRess
            existingInstances <- existingNestedInstances ress
            (clauses, childNames) <- flip runStateT mempty $ traverse (generateParseRouteClause existingInstances routeOpts) ress

            childInstances <- fmap join $ forM (Set.toList childNames) $ \childName -> do
                let targetType =
                        applyTyArgs (ConT (mkName childName)) origTyargs
                mkParseRouteInstanceOpts routeOpts { roFocusOnNestedRoute = Just childName } origTyargs cxt targetType ress

            let missingClause = Clause [WildP] (NormalB (ConE 'Nothing)) []
                allClauses = clauses <> [missingClause]

            let thisInstance =
                    case roFocusOnNestedRoute routeOpts of
                        Just target -> do
                            let targetType =
                                    applyTyArgs (ConT (mkName target)) origTyargs
                            instanceD cxt (ConT ''ParseRouteNested `AppT` targetType)
                                [ FunD 'parseRouteNested allClauses
                                ]
                        Nothing ->
                            instanceD cxt (ConT ''ParseRoute `AppT` typ)
                                [ FunD 'parseRoute allClauses
                                ]

            pure $ thisInstance : childInstances
  where
    -- Narrow to the focused subtree's children via the shared lookup, failing
    -- loudly (matching Dispatch/RenderRoute) when the target is missing rather
    -- than silently producing an always-'Nothing' 'ParseRouteNested' instance.
    focusTarget ts =
        case roFocusOnNestedRoute routeOpts of
            Just target ->
                case findNestedRoute target ts of
                    Nothing ->
                        fail $ "Target '" ++ target ++ "' was not found in resources."
                    Just (_prePieces, children) ->
                        pure children
            Nothing ->
                pure ts

-- | Probe (in 'Q') which of the given resources' top-level parents already
-- have a 'ParseRouteNested' instance. Pulling these compiler queries
-- ('isInstance', via 'nestedInstanceExists') out of 'generateParseRouteClause'
-- is what lets that generator be monad-polymorphic over 'Quote' — and thus
-- runnable, and testable, at a pure 'Quote' instance.
--
-- @since 1.7.0.0
existingNestedInstances :: [ResourceTree a] -> Q (Set.Set String)
existingNestedInstances ress = do
    flags <- mapM check ress
    pure $ Set.fromList (concat flags)
  where
    check (ResourceParent name _ _ _ _) = do
        rc <- resolveRouteCon name
        has <- nestedInstanceExists ''ParseRouteNested rc
        pure [name | has]
    check ResourceLeaf{} = pure []

-- | Generate a single @parseRouteNested@-delegating clause for one resource
-- (under nested discovery). It records, into its 'StateT' accumulator, the
-- name of each parent that still needs a 'ParseRouteNested' instance generated.
--
-- The \"does this name already have an instance?\" decision is a compiler query
-- ('isInstance'), which would pin this to 'Q'; it is instead /hoisted out/ and
-- passed in as @existingInstances@, leaving only fresh-name generation as an
-- effect. That makes the generator monad-polymorphic over 'Quote': it runs at
-- 'Q' in production and at a pure 'Quote' instance (a monotonic counter) in
-- tests.
--
-- @since 1.7.0.0
generateParseRouteClause
    :: Quote m
    => Set.Set String
    -- ^ Route names that already have a 'ParseRouteNested' instance (so this
    -- clause should not re-request one). Computed by the caller in 'Q' via
    -- 'nestedInstanceExists'; pass 'mempty' in pure tests.
    -> RouteOpts
    -> ResourceTree a
    -> StateT (Set.Set String) m Clause
generateParseRouteClause existingInstances routeOpts resourceTree =
    case resourceTree of
        ResourceLeaf (Resource name pieces dispatch _ _check) -> do
            (pats, dyns) <- liftQ $ handlePiecesM pieces

            case dispatch of
                Methods multi _ -> do
                    (finalTail, dyns') <-
                        case multi of
                            Nothing ->
                                pure (EndExact, dyns)
                            Just _ -> do
                                multiName <- liftQ $ newName "multi"
                                pure (EndMulti multiName, dyns ++ [VarE multiName])

                    queryParamsName <- liftQ $ newName "_queryParams"
                    let route = applyConPieces name dyns'
                        jroute = ConE 'Just `AppE` route
                        pathPat = mkPathPat finalTail pats
                        pat = TupP [pathPat, VarP queryParamsName]
                    pure $ Clause [pat] (NormalB jroute) []

                Subsite _ _ -> do
                    restName <- liftQ $ newName "rest"
                    queryParamsName <- liftQ $ newName "_queryParams"

                    let route = applyConPieces name dyns
                        pathPat = mkPathPat (EndRest restName) pats
                        pat = TupP [pathPat, VarP queryParamsName]
                        tupExp = mkTupE [VarE restName, VarE queryParamsName]
                        expr = VarE 'fmap
                            `AppE` route
                            `AppE` (VarE 'parseRoute `AppE` tupExp)
                    pure $ Clause [pat] (NormalB expr) []

        ResourceParent name _check _attrs pieces _children -> do
            recordNameIfNotInstance name

            (pats, dyns) <- liftQ $ handlePiecesM pieces

            let route = applyConPieces name dyns

            restName <- liftQ $ newName "rest"
            queryParamsName <- liftQ $ newName "_queryParams"

            let parseRouteOnRest =
                    VarE 'parseRouteNested
                        `AppE` mkTupE [VarE restName, VarE queryParamsName]

            body <-
                    if roNestedRouteFallthrough routeOpts
                        then do
                            resultName <- liftQ $ newName "result"
                            let stmt = BindS (AsP resultName (conPCompat 'Just [WildP])) parseRouteOnRest
                                expr = VarE 'fmap `AppE` route `AppE` VarE resultName
                            pure $ GuardedB [(PatG [stmt], expr)]
                        else
                            pure $ NormalB (VarE 'fmap `AppE` route `AppE` parseRouteOnRest)

            let pat = TupP [mkPathPat (EndRest restName) pats, VarP queryParamsName]
            pure $ Clause [pat] body []

  where
    liftQ :: Monad n => n a -> StateT (Set.Set String) n a
    liftQ = Trans.lift

    recordName :: MonadState (Set.Set String) n => String -> n ()
    recordName name =
        modify (Set.insert name)

    -- The instance-existence decision is hoisted out (into @existingInstances@,
    -- computed in 'Q' by the caller), so this stays pure.
    recordNameIfNotInstance name =
        when (not (name `Set.member` existingInstances)) $
            recordName name

-- | Backwards-compatible inline 'parseRoute' clause generation. Instead of
-- delegating nested routes to 'parseRouteNested', this inlines every nested
-- parent directly into the single 'ParseRoute' instance, wrapping each parsed
-- child route in the accumulated parent constructors. This matches the
-- historical (pre nested route discovery) output and emits no
-- 'ParseRouteNested' instances.
--
-- Crucially it preserves 1.6's /commit-on-parent-prefix/ semantics: a parent
-- contributes exactly one top-level clause that matches its path prefix once,
-- then dispatches the remaining path over its children with a 'Nothing'
-- fallback. So a request whose path matches a parent prefix but none of that
-- parent's children resolves to 'Nothing' here — it does /not/ fall through to
-- later top-level routes — which is what the inline @dispatch@ codegen does
-- too (see 'Yesod.Routes.TH.Dispatch.mkDispatchClause'). Matching the prefix
-- once also avoids re-parsing it per descendant leaf.
--
-- The inline path needs no compiler queries (@reify@\/@lookupTypeName@\/
-- @isInstance@), only fresh names, so it is just 'buildInlineParseClauses' run
-- at @m ~ 'Q'@ — production gets hygienic 'newName' binders, while the same
-- code runs at a pure 'Quote' instance (a monotonic counter) under test.
generateParseRouteClausesInline
    :: (Exp -> Exp)
    -- ^ Wrap a child-route expression in the accumulated parent constructors.
    -> ResourceTree a
    -> Q [Clause]
generateParseRouteClausesInline = buildInlineParseClauses

-- | The core of 'generateParseRouteClausesInline'. It assembles the
-- @parseRoute@ clauses for the backwards-compatible inline path directly as
-- AST, drawing fresh names from a 'Quote' name supply and building
-- tuples\/applications by hand rather than through quotation brackets
-- (brackets are monomorphic 'Q' before template-haskell 2.17; the one
-- exception goes through 'unsafeQToQuote').
--
-- Each call yields one clause per resource tree: a leaf is its own matching
-- clause, while a parent is a single clause that matches its path prefix and
-- then cases the remaining @(path, queryParams)@ over its children (their
-- clauses turned into match alternatives) ending in a @_ -> 'Nothing'@
-- fallback. That fallback is the commit-on-parent-prefix behaviour: once the
-- parent prefix matches, a child miss is a definite 'Nothing' rather than a
-- fall-through to a sibling top-level route.
--
-- It is monad-polymorphic in the name supply via 'Quote': production runs it at
-- 'Q' (so binders are hygienic 'newName's), while tests run it at a pure
-- 'Quote' instance backed by a monotonic counter, where names are deterministic
-- and the '[Clause]' output can be asserted on directly without splicing or
-- 'runQ'. Under the deterministic supply every name is a freshly-bound pattern
-- variable scoped to its clause, the shared counter keeps them distinct within a
-- tree, and it is threaded through the parent\/child recursion so a parent
-- binder never collides with a child's.
--
-- @since 1.7.0.0
buildInlineParseClauses
    :: Quote m
    => (Exp -> Exp)
    -- ^ Wrap a child-route expression in the accumulated parent constructors.
    -> ResourceTree a
    -> m [Clause]
buildInlineParseClauses wrap resourceTree =
    case resourceTree of
        ResourceLeaf (Resource name pieces dispatch _ _check) -> do
            (pats, dyns) <- handlePiecesM pieces
            case dispatch of
                Methods multi _ -> do
                    (finalTail, dyns') <-
                        case multi of
                            Nothing ->
                                pure (EndExact, dyns)
                            Just _ -> do
                                multiName <- newName "multi"
                                pure (EndMulti multiName, dyns ++ [VarE multiName])
                    queryParamsName <- newName "_queryParams"
                    let route = applyConPieces name dyns'
                        jroute = ConE 'Just `AppE` wrap route
                        pathPat = mkPathPat finalTail pats
                        pat = TupP [pathPat, VarP queryParamsName]
                    pure [Clause [pat] (NormalB jroute) []]

                Subsite _ _ -> do
                    restName <- newName "rest"
                    queryParamsName <- newName "_queryParams"
                    subName <- newName "sub"
                    let route = applyConPieces name dyns
                        wrapSub = wrap (route `AppE` VarE subName)
                        pathPat = mkPathPat (EndRest restName) pats
                        pat = TupP [pathPat, VarP queryParamsName]
                    -- 'unsafeQToQuote' because brackets are monomorphic 'Q'
                    -- on template-haskell < 2.17. It is safe here: the only
                    -- effect a bracket performs is drawing hygiene names for
                    -- its binders, and this one has none (the lambda binder
                    -- comes from a splice).
                    expr <- unsafeQToQuote [e|
                        fmap
                            (\ $(varP subName) -> $(pure wrapSub) )
                            (parseRoute ( $(varE restName), $(varE queryParamsName) ) )
                        |]
                    pure [Clause [pat] (NormalB expr) []]

        ResourceParent name _check _attrs pieces children -> do
            (pats, dyns) <- handlePiecesM pieces
            restName <- newName "rest"
            queryParamsName <- newName "_queryParams"
            let parentCon childRoute =
                    applyConPieces name dyns `AppE` childRoute
                wrap' = wrap . parentCon

            -- Build each child's matching clause relative to the remaining
            -- @(rest, queryParams)@ scope, then fold them into the alternatives
            -- of a single @case@ that commits at this parent: a child miss
            -- falls to the @_ -> Nothing@ alternative instead of escaping to a
            -- sibling top-level route.
            childClauses <- concat <$> mapM (buildInlineParseClauses wrap') children
            let restTup = mkTupE [VarE restName, VarE queryParamsName]
                childMatches = map clauseToMatch childClauses
                fallbackMatch = Match WildP (NormalB (ConE 'Nothing)) []
                caseExpr = CaseE restTup (childMatches ++ [fallbackMatch])
                pathPat = mkPathPat (EndRest restName) pats
                pat = TupP [pathPat, VarP queryParamsName]
            pure [Clause [pat] (NormalB caseExpr) []]
  where
    -- A child clause matches the @(path, queryParams)@ tuple with no @where@
    -- bindings, so it converts directly to a @case@ alternative. (Assert the
    -- shape we rely on rather than silently dropping non-empty @where@ decls.)
    clauseToMatch :: Clause -> Match
    clauseToMatch (Clause [pat] body []) = Match pat body []
    clauseToMatch c =
        error $ "buildInlineParseClauses: unexpected child clause shape: " <> show c
