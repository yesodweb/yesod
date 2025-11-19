{-# LANGUAGE TemplateHaskell #-}
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
mkParseRouteInstanceOpts routeOpts cxt typ ress = do
--     (childNames, cls) <- mkDispatchClause
--         MkDispatchSettings
--             { mdsRunHandler = [|\_ _ x _ -> x|]
--             , mds404 = [|error "mds404"|]
--             , mds405 = [|error "mds405"|]
--             , mdsGetPathInfo = [|fst|]
--             , mdsMethod = [|error "mdsMethod"|]
--             , mdsGetHandler = \_ _ -> [|error "mdsGetHandler"|]
--             , mdsSetPathInfo = [|\path (_, queryParams) -> (path, queryParams)|]
--             , mdsSubDispatcher = [|\_runHandler _getSub toMaster _env -> fmap toMaster . parseRoute|]
--             , mdsUnwrapper = return
--             , mdsHandleNestedRoute = Just NestedRouteSettings
--                 { nrsClassName = ''ParseRouteNested
--                 , nrsDispatchCall = \restExpr sdc constrExpr _dyns ->
--                     [e| fmap $(pure constrExpr) (parseRouteNested ($(pure restExpr), snd $(pure $ reqExp sdc))) |]
--                 , nrsTargetName = Nothing
--                 , nrsWrapDispatchCall = \_ _ e -> [|Just $(pure e)|]
--                 }
--             , mdsNestedRouteFallthrough = False
--             }
--         (map removeMethods ress)

    (clauses, childNames) <- flip runStateT mempty $ traverse go ress

    childInstances <- fmap join $ forM (Set.toList childNames) $ \childName ->
        mkParseRouteInstanceFor childName ress

    return
        $ [instanceD cxt (ConT ''ParseRoute `AppT` typ)
            [ FunD 'parseRoute (clauses ++ [Clause [WildP] (NormalB (ConE 'Nothing)) []])
            ]
          ] <> childInstances
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

    go
        :: ResourceTree a
        -> StateT (Set.Set String) Q Clause
    go (ResourceLeaf (Resource name pieces dispatch _ _check)) = do
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
                -- TODO: support fully.
                -- mdsSubDispatcher ~ \_runHandler _getSub toMaster _env ->
                --
                -- toMaster uses the accumulated constructors. see
                -- handleDispatch code in Yesod.Routes.TH.Dispatch

                restName <- newName "rest"
                queryParamsName <- newName "_queryParams"

                let route = List.foldl' AppE (ConE (mkName name)) dyns
                    pathPat = mkPathPat (VarP restName) pats

                pat <- [p| ($(pure pathPat), $(pure (VarP queryParamsName)) ) |]
                tupExp <- [e| ( $(pure $ VarE restName), $(pure $ VarE queryParamsName) ) |]
                expr <- [e| fmap $(pure route) ( parseRoute $(pure tupExp) ) |]
                pure $ Clause [pat] (NormalB expr) []

    go (ResourceParent name _check pieces _children) = do
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
                        pure $ GuardedB [(PatG [stmt], VarE resultName)]
                    else do
                        expr <- [e| fmap $(pure route) ( $(pure parseRouteOnRest) ) |]
                        pure $ NormalB expr

        pat <- [p| ($(pure (mkPathPat (VarP restName) pats)), $(pure (VarP queryParamsName)) ) |]
        pure $ Clause [pat] body []

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
mkParseRouteInstanceFor target ress = do
    (childNames, cls) <- mkDispatchClause
        MkDispatchSettings
            { mdsRunHandler = [|\_ _ x _ -> x|]
            , mds404 = [|error "mds404"|]
            , mds405 = [|error "mds405"|]
            , mdsGetPathInfo = [|fst|]
            , mdsMethod = [|error "mdsMethod"|]
            , mdsGetHandler = \_ _ -> [|error "mdsGetHandler"|]
            , mdsSetPathInfo = [|\p (_, q) -> (p, q)|]
            , mdsSubDispatcher = [|\_runHandler _getSub toMaster _env -> fmap toMaster . parseRoute|]
            , mdsUnwrapper = return
            , mdsHandleNestedRoute = Just NestedRouteSettings
                { nrsClassName = ''ParseRouteNested
                , nrsDispatchCall = \restExpr sdc constrExpr _dyns ->
                    [e| fmap $(pure constrExpr) (parseRouteNested ($(pure restExpr), snd $(pure $ reqExp sdc))) |]
                , nrsTargetName = Just target
                , nrsWrapDispatchCall = \_ _ expr -> [|Just $(pure expr)|]
                }
            , mdsNestedRouteFallthrough = False
            }
        (focusTarget (map removeMethods ress))
    helper <- newName "helper"
    fixer <- [|(\f x -> f () x) :: (() -> ([Text], [(Text, Text)]) -> Maybe a) -> ([Text], [(Text, Text)]) -> Maybe a|]
    return $ [instanceD [] (ConT ''ParseRouteNested `AppT` ConT (mkName target))
        [ FunD 'parseRouteNested $ return $ Clause
            []
            (NormalB $ fixer `AppE` VarE helper)
            [FunD helper [cls]]
        ]
        ]
  where
    focusTarget =
        foldr k []
      where
        k res acc =
            case res of
                ResourceLeaf _ ->
                    acc
                ResourceParent name _ _ children ->
                    if name == target
                    then res : acc
                    else focusTarget children <> acc

    -- We do this in order to skip the unnecessary method parsing
    removeMethods (ResourceLeaf res) = ResourceLeaf $ removeMethodsLeaf res
    removeMethods (ResourceParent w x y z) = ResourceParent w x y $ map removeMethods z

    removeMethodsLeaf res = res { resourceDispatch = fixDispatch $ resourceDispatch res }

    fixDispatch (Methods x _) = Methods x []
    fixDispatch x = x

instanceD :: Cxt -> Type -> [Dec] -> Dec
instanceD = InstanceD Nothing

instance Quote (StateT s Q) where
    newName = Trans.lift . newName
