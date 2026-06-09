{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Yesod.Routes.TH.ParseRoute
    ( -- ** ParseRoute
      mkParseRouteInstance
    , mkParseRouteInstanceOpts
    , mkParseRouteInstanceFor
      -- ** Clause construction (monad-polymorphic; testable at @State Int@)
    , buildInlineParseClauses
    , generateParseRouteClause
    , FreshName(..)
    ) where

import qualified Control.Monad.Trans as Trans
import qualified Data.Set as Set
import Control.Monad
import Yesod.Routes.TH.Types
import Language.Haskell.TH.Syntax
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
    let opts = setFocusOnNestedRoute (Just target) defaultOpts
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
            clausess <- mapM (generateParseRouteClausesInline [] id) unfocusedRess
            let missingClause = Clause [WildP] (NormalB (ConE 'Nothing)) []
                allClauses = concat clausess <> [missingClause]
            pure
                [ instanceD cxt (ConT ''ParseRoute `AppT` typ)
                    [ FunD 'parseRoute allClauses
                    ]
                ]
        NestedDiscovery -> do
            let ress = focusTarget unfocusedRess
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
    focusTarget ts =
        case roFocusOnNestedRoute routeOpts of
            Just target ->
                foldr k [] ts
              where
                k res acc =
                    case res of
                        ResourceLeaf _ ->
                            acc
                        ResourceParent name _ _ _ children ->
                            if name == target
                            then children
                            else focusTarget children <> acc
            Nothing ->
                ts

-- | Probe (in 'Q') which of the given resources' top-level parents already
-- have a 'ParseRouteNested' instance. Pulling these compiler queries
-- ('isInstance', via 'nestedInstanceExists') out of 'generateParseRouteClause'
-- is what lets that generator be monad-polymorphic — and thus runnable, and
-- testable, at @'State' 'Int'@.
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
-- effect. That makes the generator monad-polymorphic via 'FreshName': it runs
-- at 'Q' in production and at @'State' 'Int'@ in tests.
--
-- @since 1.7.0.0
generateParseRouteClause
    :: FreshName m
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
                                multiName <- liftQ $ freshName "multi"
                                pure (EndMulti multiName, dyns ++ [VarE multiName])

                    queryParamsName <- liftQ $ freshName "_queryParams"
                    let route = applyConPieces name dyns'
                        jroute = ConE 'Just `AppE` route
                        pathPat = mkPathPat finalTail pats
                        pat = TupP [pathPat, VarP queryParamsName]
                    pure $ Clause [pat] (NormalB jroute) []

                Subsite _ _ -> do
                    restName <- liftQ $ freshName "rest"
                    queryParamsName <- liftQ $ freshName "_queryParams"

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

            restName <- liftQ $ freshName "rest"
            queryParamsName <- liftQ $ freshName "_queryParams"

            let parseRouteOnRest =
                    VarE 'parseRouteNested
                        `AppE` mkTupE [VarE restName, VarE queryParamsName]

            body <-
                    if roNestedRouteFallthrough routeOpts
                        then do
                            resultName <- liftQ $ freshName "result"
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
-- delegating nested routes to 'parseRouteNested', this recursively inlines
-- every descendant leaf into a single flat list of clauses, prepending the
-- accumulated parent path pieces and wrapping the parsed route in the
-- accumulated parent constructors. This matches the historical (pre nested
-- route discovery) output and emits no 'ParseRouteNested' instances.
--
-- The inline path needs no compiler queries (@reify@\/@lookupTypeName@\/
-- @isInstance@), only fresh names, so it is just 'buildInlineParseClauses' run
-- at @m ~ 'Q'@ — production gets hygienic 'newName' binders, while the same
-- code runs at @'State' 'Int'@ under test.
generateParseRouteClausesInline
    :: [Pat]
    -- ^ Accumulated parent path-piece patterns (binding the parent dynamics).
    -> (Exp -> Exp)
    -- ^ Wrap a child-route expression in the accumulated parent constructors.
    -> ResourceTree a
    -> Q [Clause]
generateParseRouteClausesInline = buildInlineParseClauses

-- | The core of 'generateParseRouteClausesInline'. It assembles the
-- @parseRoute@ clauses for the backwards-compatible inline path directly as
-- AST, drawing fresh names from a 'FreshName' supply and building
-- tuples\/applications by hand instead of through quotation brackets.
--
-- It is monad-polymorphic in the name supply: production runs it at 'Q' (so
-- binders are hygienic 'newName's), while tests run it at @'State' 'Int'@,
-- where names are a deterministic counter and the '[Clause]' output can be
-- asserted on directly without splicing or 'runQ'. Under the deterministic
-- supply every name is a freshly-bound pattern variable scoped to its clause,
-- the shared counter keeps them distinct within a tree, and it is threaded
-- through the parent\/child recursion so an accumulated parent binder never
-- collides with a child's.
--
-- @since 1.7.0.0
buildInlineParseClauses
    :: FreshName m
    => [Pat]
    -- ^ Accumulated parent path-piece patterns (binding the parent dynamics).
    -> (Exp -> Exp)
    -- ^ Wrap a child-route expression in the accumulated parent constructors.
    -> ResourceTree a
    -> m [Clause]
buildInlineParseClauses accPats wrap resourceTree =
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
                                multiName <- freshName "multi"
                                pure (EndMulti multiName, dyns ++ [VarE multiName])
                    queryParamsName <- freshName "_queryParams"
                    let route = applyConPieces name dyns'
                        jroute = ConE 'Just `AppE` wrap route
                        pathPat = mkPathPat finalTail (accPats ++ pats)
                        pat = TupP [pathPat, VarP queryParamsName]
                    pure [Clause [pat] (NormalB jroute) []]

                Subsite _ _ -> do
                    restName <- freshName "rest"
                    queryParamsName <- freshName "_queryParams"
                    subName <- freshName "sub"
                    let route = applyConPieces name dyns
                        wrapSub = wrap (route `AppE` VarE subName)
                        pathPat = mkPathPat (EndRest restName) (accPats ++ pats)
                        pat = TupP [pathPat, VarP queryParamsName]
                        tupExp = mkTupE [VarE restName, VarE queryParamsName]
                        expr = VarE 'fmap
                            `AppE` LamE [VarP subName] wrapSub
                            `AppE` (VarE 'parseRoute `AppE` tupExp)
                    pure [Clause [pat] (NormalB expr) []]

        ResourceParent name _check _attrs pieces children -> do
            (pats, dyns) <- handlePiecesM pieces
            let accPats' = accPats ++ pats
                parentCon childRoute =
                    applyConPieces name dyns `AppE` childRoute
                wrap' = wrap . parentCon
            concat <$> mapM (buildInlineParseClauses accPats' wrap') children
