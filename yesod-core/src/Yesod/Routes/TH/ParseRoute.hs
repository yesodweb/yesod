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

-- | The trailing @_ -> Nothing@ clause appended to every generated
-- @parseRoute@\/@parseRouteNested@: a path matching none of the preceding
-- clauses fails to parse.
missingRouteClause :: Clause
missingRouteClause = Clause [WildP] (NormalB (ConE 'Nothing)) []

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
    case discoveryMode routeOpts origTyargs of
        -- Backwards-compatible default: generate a single 'ParseRoute'
        -- instance with all nested routes inlined, and no 'ParseRouteNested'
        -- instances.
        InlineCompat -> do
            clausess <- mapM (buildInlineParseClauses id) unfocusedRess
            let allClauses = concat clausess <> [missingRouteClause]
            pure
                [ instanceD cxt (ConT ''ParseRoute `AppT` typ)
                    [ FunD 'parseRoute allClauses
                    ]
                ]
        NestedDiscovery -> do
            ress <- focusTarget unfocusedRess
            existingInstances <- existingNestedInstances ress
            (clauses, childNames) <- flip runStateT mempty $ traverse (generateParseRouteClause existingInstances routeOpts) ress

            childInstances <- fmap join $ forM (Set.toList childNames) $ \childName ->
                mkParseRouteInstanceOpts routeOpts { roFocusOnNestedRoute = Just childName } origTyargs cxt (targetTypeFor childName) ress

            let allClauses = clauses <> [missingRouteClause]

            let thisInstance =
                    case roFocusOnNestedRoute routeOpts of
                        Just target ->
                            instanceD cxt (ConT ''ParseRouteNested `AppT` targetTypeFor target)
                                [ FunD 'parseRouteNested allClauses
                                ]
                        Nothing ->
                            instanceD cxt (ConT ''ParseRoute `AppT` typ)
                                [ FunD 'parseRoute allClauses
                                ]

            pure $ thisInstance : childInstances
  where
    -- The route datatype named by @name@, applied to the site's type arguments.
    targetTypeFor name = applyTyArgs (ConT (mkName name)) origTyargs

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

    -- Record that @name@ still needs a 'ParseRouteNested' instance generated.
    -- The instance-existence decision is hoisted out (into @existingInstances@,
    -- computed in 'Q' by the caller), so this stays pure.
    recordNameIfNotInstance name =
        unless (name `Set.member` existingInstances) $
            modify (Set.insert name)

-- | Backwards-compatible inline @parseRoute@ clause generation. It assembles
-- the @parseRoute@ clauses for the inline path directly as AST, drawing fresh
-- names from a 'Quote' name supply and building tuples\/applications by hand
-- rather than through quotation brackets
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
    map matchToClause <$> buildInlineParseMatches wrap resourceTree
  where
    -- Each parse match is a single @(path, queryParams)@ pattern with a body,
    -- which is exactly a top-level @parseRoute@ clause. Total by construction —
    -- 'Match' and 'Clause' carry the same body\/@where@ shape — so no partial
    -- destructure is needed.
    matchToClause :: Match -> Clause
    matchToClause (Match pat body decs) = Clause [pat] body decs

-- | The matching alternatives behind 'buildInlineParseClauses', one per
-- resource tree. Producing 'Match'es (rather than 'Clause's) is what lets a
-- parent splice its children straight into a @case@: a leaf is one
-- @(path, queryParams)@ alternative, and a parent is one alternative that
-- matches its path prefix then cases the remaining @(path, queryParams)@ over
-- its children's alternatives, ending in a @_ -> 'Nothing'@ fallback. At top
-- level 'buildInlineParseClauses' turns each alternative back into its own
-- @parseRoute@ clause.
buildInlineParseMatches
    :: Quote m
    => (Exp -> Exp)
    -- ^ Wrap a child-route expression in the accumulated parent constructors.
    -> ResourceTree a
    -> m [Match]
buildInlineParseMatches wrap resourceTree =
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
                    pure [Match pat (NormalB jroute) []]

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
                    pure [Match pat (NormalB expr) []]

        ResourceParent name _check _attrs pieces children -> do
            (pats, dyns) <- handlePiecesM pieces
            restName <- newName "rest"
            queryParamsName <- newName "_queryParams"
            let parentCon childRoute =
                    applyConPieces name dyns `AppE` childRoute
                wrap' = wrap . parentCon

            -- Build each child's matching alternative relative to the remaining
            -- @(rest, queryParams)@ scope, then fold them into a single @case@
            -- that commits at this parent: a child miss falls to the
            -- @_ -> Nothing@ alternative instead of escaping to a sibling
            -- top-level route.
            childMatches <- concat <$> mapM (buildInlineParseMatches wrap') children
            let restTup = mkTupE [VarE restName, VarE queryParamsName]
                fallbackMatch = Match WildP (NormalB (ConE 'Nothing)) []
                caseExpr = CaseE restTup (childMatches ++ [fallbackMatch])
                pathPat = mkPathPat (EndRest restName) pats
                pat = TupP [pathPat, VarP queryParamsName]
            pure [Match pat (NormalB caseExpr) []]
