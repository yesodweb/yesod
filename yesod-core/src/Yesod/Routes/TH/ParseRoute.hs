{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Yesod.Routes.TH.ParseRoute
    ( -- ** ParseRoute
      mkParseRouteInstance
    , mkParseRouteInstanceOpts
    , mkParseRouteInstanceFor
    ) where

import qualified Data.List as List
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
import Control.Arrow (second)
import Data.Maybe

mkParseRouteInstance :: [(Type, Name)] -> Cxt -> Type -> [ResourceTree a] -> Q [Dec]
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
    mkParseRouteInstanceOpts opts [] [] targetType ress

mkParseRouteInstanceOpts :: RouteOpts -> [(Type, Name)] -> Cxt -> Type -> [ResourceTree a] -> Q [Dec]
mkParseRouteInstanceOpts routeOpts origTyargs cxt typ unfocusedRess = do
    let ress = focusTarget unfocusedRess
    (clauses, childNames) <- flip runStateT mempty $ traverse (generateParseRouteClause routeOpts) ress

    childInstances <- fmap join $ forM (Set.toList childNames) $ \childName -> do
        let targetType =
                List.foldl' (\t x -> t `AppT` fst x) (ConT (mkName childName)) origTyargs
        mkParseRouteInstanceOpts routeOpts { roFocusOnNestedRoute = Just childName } origTyargs cxt targetType ress

    let missingClause = Clause [WildP] (NormalB (ConE 'Nothing)) []
        allClauses = clauses <> [missingClause]

    let thisInstance =
            case roFocusOnNestedRoute routeOpts of
                Just target -> do
                    let targetType =
                            List.foldl' (\t x -> t `AppT` fst x) (ConT (mkName target)) origTyargs
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
            (pats, dyns) <- handlePieces pieces

            case dispatch of
                Methods multi _ -> do
                    (finalPat, dyns') <-
                        case multi of
                            Nothing -> do
                                pure (conPCompat '[] [], dyns)
                            Just _ -> do
                                multiName <- liftQ $ newName "multi"
                                let pat = ViewP (VarE 'fromPathMultiPiece)
                                                (conPCompat 'Just [VarP multiName])
                                pure (pat, dyns ++ [VarE multiName])

                    let route = List.foldl' AppE (ConE (mkName name)) dyns'
                        jroute = ConE 'Just `AppE` route
                        pathPat = mkPathPat finalPat pats
                    queryParamsName <- liftQ $ newName "_queryParams"
                    pat <- liftQ [p| ($(pure pathPat), $(pure (VarP queryParamsName)) ) |]
                    pure $ Clause [pat] (NormalB jroute) []

                Subsite _ _ -> do
                    restName <- liftQ $ newName "rest"
                    queryParamsName <- liftQ $ newName "_queryParams"

                    let route = List.foldl' AppE (ConE (mkName name)) dyns
                        pathPat = mkPathPat (VarP restName) pats

                    pat <- liftQ [p| ($(pure pathPat), $(pure (VarP queryParamsName)) ) |]
                    tupExp <- liftQ [e| ( $(pure $ VarE restName), $(pure $ VarE queryParamsName) ) |]
                    expr <- liftQ [e| fmap $(pure route) ( parseRoute $(pure tupExp) ) |]
                    pure $ Clause [pat] (NormalB expr) []

        ResourceParent name _check _attrs pieces _children -> do
            recordNameIfNotInstance name

            (pats, dyns) <- handlePieces pieces

            let route = List.foldl' AppE (ConE (mkName name)) dyns

            restName <- liftQ $ newName "rest"
            queryParamsName <- liftQ $ newName "_queryParams"

            parseRouteOnRest <- liftQ [e| parseRouteNested ( $(pure $ VarE restName), $(pure $ VarE queryParamsName)) |]

            body <-
                    if roNestedRouteFallthrough routeOpts
                        then do
                            resultName <- liftQ $ newName "result"
                            let stmt = BindS (AsP resultName (conPCompat 'Just [WildP])) parseRouteOnRest
                                route' = List.foldl' AppE (ConE (mkName name)) dyns
                            expr <- liftQ [e| fmap $(pure route') ( $(pure (VarE resultName)) ) |]
                            pure $ GuardedB [(PatG [stmt], expr)]
                        else do
                            expr <- liftQ [e| fmap $(pure route) ( $(pure parseRouteOnRest) ) |]
                            pure $ NormalB expr

            pat <- liftQ [p| ($(pure (mkPathPat (VarP restName) pats)), $(pure (VarP queryParamsName)) ) |]
            pure $ Clause [pat] body []

  where
    liftQ :: Q a -> StateT s Q a
    liftQ = Trans.lift

    recordName :: MonadState (Set.Set String) m => String -> m ()
    recordName name =
        modify (Set.insert name)

    recordNameIfNotInstance name = do
        mtypeName <- liftQ $ lookupTypeName name
        case mtypeName of
            Nothing ->
                recordName name
            Just typeName -> do
                appliedT <- liftQ $ fullyApplyType typeName
                hasNestedInstance <- liftQ $ isInstance ''ParseRouteNested [appliedT]
                when (not hasNestedInstance) $ do
                    recordName name

    mkPathPat :: Pat -> [Pat] -> Pat
    mkPathPat final =
        foldr addPat final
      where
        addPat x y = conPCompat '(:) [x, y]

    handlePiece :: Piece a -> StateT s Q (Pat, Maybe Exp)
    handlePiece (Static str) = return (LitP $ StringL str, Nothing)
    handlePiece (Dynamic _) = do
        x <- liftQ $ newName "dyn"
        let pat = ViewP (VarE 'fromPathPiece) (conPCompat 'Just [VarP x])
        return (pat, Just $ VarE x)

    handlePieces :: [Piece a] -> StateT s Q ([Pat], [Exp])
    handlePieces = fmap (second catMaybes . unzip) . mapM handlePiece
