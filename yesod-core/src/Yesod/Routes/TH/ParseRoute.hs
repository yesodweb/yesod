{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Yesod.Routes.TH.ParseRoute
    ( -- ** ParseRoute
      mkParseRouteInstance
    , mkParseRouteInstanceOpts
    , mkParseRouteInstanceFor
      -- ** Pure clause construction (for testing)
    , buildInlineParseClauses
    ) where

import qualified Control.Monad.Trans as Trans
import qualified Data.Set as Set
import Control.Monad
import Yesod.Routes.TH.Types
import Language.Haskell.TH.Syntax
import Yesod.Routes.Class
import Yesod.Routes.TH.RenderRoute
import Control.Monad.State.Strict
import Web.PathPieces
import Yesod.Routes.TH.Internal

mkParseRouteInstance :: TyArgs -> Cxt -> Type -> [ResourceTree a] -> Q [Dec]
mkParseRouteInstance =
    mkParseRouteInstanceOpts defaultOpts

-- | Generate a ParseRouteNested instance for a specific nested route target.
-- This is a convenience wrapper around mkParseRouteInstanceOpts with setFocusOnNestedRoute.
--
-- @since 1.6.28.0
mkParseRouteInstanceFor :: String -> [ResourceTree a] -> Q [Dec]
mkParseRouteInstanceFor target ress = do
    let opts = setFocusOnNestedRoute (Just target) defaultOpts
        targetType = ConT (mkName target)
    mkParseRouteInstanceOpts opts NoTyArgs [] targetType ress

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
            (clauses, childNames) <- flip runStateT mempty $ traverse (generateParseRouteClause routeOpts) ress

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

generateParseRouteClause
    :: RouteOpts
    -> ResourceTree a
    -> StateT (Set.Set String) Q Clause
generateParseRouteClause routeOpts resourceTree =
    case resourceTree of
        ResourceLeaf (Resource name pieces dispatch _ _check) -> do
            (pats, dyns) <- liftQ $ handlePiecesM newName pieces

            case dispatch of
                Methods multi _ -> do
                    (finalTail, dyns') <-
                        case multi of
                            Nothing -> do
                                pure (EndExact, dyns)
                            Just _ -> do
                                multiName <- liftQ $ newName "multi"
                                pure (EndMulti multiName, dyns ++ [VarE multiName])

                    let route = applyConPieces name dyns'
                        jroute = ConE 'Just `AppE` route
                        pathPat = mkPathPat finalTail pats
                    queryParamsName <- liftQ $ newName "_queryParams"
                    pat <- liftQ [p| ($(pure pathPat), $(pure (VarP queryParamsName)) ) |]
                    pure $ Clause [pat] (NormalB jroute) []

                Subsite _ _ -> do
                    restName <- liftQ $ newName "rest"
                    queryParamsName <- liftQ $ newName "_queryParams"

                    let route = applyConPieces name dyns
                        pathPat = mkPathPat (EndRest restName) pats

                    pat <- liftQ [p| ($(pure pathPat), $(pure (VarP queryParamsName)) ) |]
                    tupExp <- liftQ [e| ( $(pure $ VarE restName), $(pure $ VarE queryParamsName) ) |]
                    expr <- liftQ [e| fmap $(pure route) ( parseRoute $(pure tupExp) ) |]
                    pure $ Clause [pat] (NormalB expr) []

        ResourceParent name _check _attrs pieces _children -> do
            recordNameIfNotInstance name

            (pats, dyns) <- liftQ $ handlePiecesM newName pieces

            let route = applyConPieces name dyns

            restName <- liftQ $ newName "rest"
            queryParamsName <- liftQ $ newName "_queryParams"

            parseRouteOnRest <- liftQ [e| parseRouteNested ( $(pure $ VarE restName), $(pure $ VarE queryParamsName)) |]

            body <-
                    if roNestedRouteFallthrough routeOpts
                        then do
                            resultName <- liftQ $ newName "result"
                            let stmt = BindS (AsP resultName (conPCompat 'Just [WildP])) parseRouteOnRest
                                route' = applyConPieces name dyns
                            expr <- liftQ [e| fmap $(pure route') ( $(pure (VarE resultName)) ) |]
                            pure $ GuardedB [(PatG [stmt], expr)]
                        else do
                            expr <- liftQ [e| fmap $(pure route) ( $(pure parseRouteOnRest) ) |]
                            pure $ NormalB expr

            pat <- liftQ [p| ($(pure (mkPathPat (EndRest restName) pats)), $(pure (VarP queryParamsName)) ) |]
            pure $ Clause [pat] body []

  where
    liftQ :: Q a -> StateT s Q a
    liftQ = Trans.lift

    recordName :: MonadState (Set.Set String) m => String -> m ()
    recordName name =
        modify (Set.insert name)

    recordNameIfNotInstance name = do
        hasNestedInstance <-
            liftQ $ nestedInstanceExists ''ParseRouteNested =<< resolveRouteCon name
        when (not hasNestedInstance) $
            recordName name

-- | Backwards-compatible inline 'parseRoute' clause generation. Instead of
-- delegating nested routes to 'parseRouteNested', this recursively inlines
-- every descendant leaf into a single flat list of clauses, prepending the
-- accumulated parent path pieces and wrapping the parsed route in the
-- accumulated parent constructors. This matches the historical (pre nested
-- route discovery) output and emits no 'ParseRouteNested' instances.
--
-- This is a thin 'Q' shell: the inline path needs no compiler queries
-- (@reify@\/@lookupTypeName@\/@isInstance@), only fresh names, so all of the
-- AST assembly lives in the pure 'buildInlineParseClauses' below and this just
-- runs it.
generateParseRouteClausesInline
    :: [Pat]
    -- ^ Accumulated parent path-piece patterns (binding the parent dynamics).
    -> (Exp -> Exp)
    -- ^ Wrap a child-route expression in the accumulated parent constructors.
    -> ResourceTree a
    -> Q [Clause]
generateParseRouteClausesInline accPats wrap resourceTree =
    pure $ evalState (buildInlineParseClauses accPats wrap resourceTree) 0

-- | A deterministic fresh-name supply: a monotonically increasing 'Int'
-- counter, each value yielding a distinct name. Used in place of 'Q'\'s
-- 'newName' so 'buildInlineParseClauses' can be pure.
freshName :: String -> State Int Name
freshName base = state $ \n -> (mkName (base ++ show n), n + 1)

-- | The effect-free core of 'generateParseRouteClausesInline'. It assembles the
-- @parseRoute@ clauses for the backwards-compatible inline path directly as
-- AST, taking a deterministic fresh-name supply (a 'State' 'Int' counter)
-- rather than 'Q'\'s 'newName', and building tuples\/applications by hand
-- instead of through quotation brackets.
--
-- The deterministic names are safe: every name is a freshly-bound pattern
-- variable scoped to the clause it appears in, the single shared counter makes
-- each generated name globally distinct within a tree, and the counter is
-- threaded through the parent\/child recursion so an accumulated parent binder
-- never collides with a child's. Because it is pure, it is directly
-- unit-testable on its '[Clause]' output without splicing or 'runQ'.
buildInlineParseClauses
    :: [Pat]
    -- ^ Accumulated parent path-piece patterns (binding the parent dynamics).
    -> (Exp -> Exp)
    -- ^ Wrap a child-route expression in the accumulated parent constructors.
    -> ResourceTree a
    -> State Int [Clause]
buildInlineParseClauses accPats wrap resourceTree =
    case resourceTree of
        ResourceLeaf (Resource name pieces dispatch _ _check) -> do
            (pats, dyns) <- handlePiecesM freshName pieces
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
            (pats, dyns) <- handlePiecesM freshName pieces
            let accPats' = accPats ++ pats
                parentCon childRoute =
                    applyConPieces name dyns `AppE` childRoute
                wrap' = wrap . parentCon
            concat <$> mapM (buildInlineParseClauses accPats' wrap') children
