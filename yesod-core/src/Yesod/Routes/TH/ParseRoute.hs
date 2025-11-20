{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

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
import Data.Text (Text)
import Yesod.Routes.Class
import Yesod.Routes.TH.Dispatch
import Yesod.Routes.TH.RenderRoute
import Control.Monad.State.Strict
import Web.PathPieces
import Yesod.Routes.TH.Internal
import Control.Arrow (second)
import Data.Maybe

mkParseRouteInstance :: Cxt -> Type -> [ResourceTree a] -> Q [Dec]
mkParseRouteInstance =
    mkParseRouteInstanceOpts defaultOpts

mkParseRouteInstanceOpts :: RouteOpts -> Cxt -> Type -> [ResourceTree a] -> Q [Dec]
mkParseRouteInstanceOpts routeOpts cxt typ unfocusedRess = do
    let ress = focusTarget unfocusedRess
    (clauses, childNames) <- flip runStateT mempty $ traverse (generateParseRouteClause routeOpts) ress

    childInstances <- fmap join $ forM (Set.toList childNames) $ \childName ->
        mkParseRouteInstanceOpts routeOpts { roFocusOnNestedRoute = Just childName } cxt (ConT (mkName childName)) ress

    let missingClause = Clause [WildP] (NormalB (ConE 'Nothing)) []
        allClauses = clauses <> [missingClause]

    let thisInstance =
            case roFocusOnNestedRoute routeOpts of
                Just target ->
                    instanceD [] (ConT ''ParseRouteNested `AppT` ConT (mkName target))
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
                        ResourceParent name _ _ children ->
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
                                multiName <- newName "multi"
                                let pat = ViewP (VarE 'fromPathMultiPiece)
                                                (conPCompat 'Just [VarP multiName])
                                pure (pat, dyns ++ [VarE multiName])

                    let route = List.foldl' AppE (ConE (mkName name)) dyns'
                        jroute = ConE 'Just `AppE` route
                        pathPat = mkPathPat finalPat pats
                    queryParamsName <- newName "_queryParams"
                    pat <- [p| ($(pure pathPat), $(pure (VarP queryParamsName)) ) |]
                    pure $ Clause [pat] (NormalB jroute) []

                Subsite _ _ -> do
                    restName <- newName "rest"
                    queryParamsName <- newName "_queryParams"

                    let route = List.foldl' AppE (ConE (mkName name)) dyns
                        pathPat = mkPathPat (VarP restName) pats

                    pat <- [p| ($(pure pathPat), $(pure (VarP queryParamsName)) ) |]
                    tupExp <- [e| ( $(pure $ VarE restName), $(pure $ VarE queryParamsName) ) |]
                    expr <- [e| fmap $(pure route) ( parseRoute $(pure tupExp) ) |]
                    pure $ Clause [pat] (NormalB expr) []

        ResourceParent name _check pieces _children -> do
            recordNameIfNotInstance name

            (pats, dyns) <- handlePieces pieces

            let route = List.foldl' AppE (ConE (mkName name)) dyns

            restName <- newName "rest"
            queryParamsName <- newName "_queryParams"

            parseRouteOnRest <- [e| parseRouteNested ( $(pure $ VarE restName), $(pure $ VarE queryParamsName)) |]

            body <-
                    if roNestedRouteFallthrough routeOpts
                        then do
                            resultName <- newName "result"
                            let stmt = BindS (AsP resultName (conPCompat 'Just [WildP])) parseRouteOnRest
                                route = List.foldl' AppE (ConE (mkName name)) dyns
                            expr <- [e| fmap $(pure route) ( $(pure (VarE resultName)) ) |]
                            pure $ GuardedB [(PatG [stmt], expr)]
                        else do
                            expr <- [e| fmap $(pure route) ( $(pure parseRouteOnRest) ) |]
                            pure $ NormalB expr

            pat <- [p| ($(pure (mkPathPat (VarP restName) pats)), $(pure (VarP queryParamsName)) ) |]
            pure $ Clause [pat] body []

  where
    recordName :: MonadState (Set.Set String) m => String -> m ()
    recordName name =
        modify (Set.insert name)

    recordNameIfNotInstance name = do
        mtypeName <- Trans.lift $ lookupTypeName name
        case mtypeName of
            Nothing ->
                recordName name
            Just typeName -> do
                hasNestedInstance <- Trans.lift $ isInstance ''ParseRouteNested [ConT typeName]
                when (not hasNestedInstance) $ do
                    recordName name

    mkPathPat :: Pat -> [Pat] -> Pat
    mkPathPat final =
        foldr addPat final
      where
        addPat x y = conPCompat '(:) [x, y]

    handlePiece :: Quote m => Piece a -> m (Pat, Maybe Exp)
    handlePiece (Static str) = return (LitP $ StringL str, Nothing)
    handlePiece (Dynamic _) = do
        x <- newName "dyn"
        let pat = ViewP (VarE 'fromPathPiece) (conPCompat 'Just [VarP x])
        return (pat, Just $ VarE x)

    handlePieces :: Quote m => [Piece a] -> m ([Pat], [Exp])
    handlePieces = fmap (second catMaybes . unzip) . mapM handlePiece

data AccumulatedParams = AccumulatedParams
    { accumulatedParameters :: [Exp]
    , accumulatedConstructors :: [Exp]
    }

mkParseRouteInstanceFor :: String -> [ResourceTree a] -> Q [Dec]
mkParseRouteInstanceFor target =
    mkParseRouteInstanceOpts
        defaultOpts { roFocusOnNestedRoute = Just target }
        []
        (ConT (mkName target))

instance Quote (StateT s Q) where
    newName = Trans.lift . newName
