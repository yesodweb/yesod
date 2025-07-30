{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Yesod.Routes.TH.Dispatch
    ( MkDispatchSettings (..)
    , mkDispatchClause
    , defaultGetHandler
    , NestedRouteSettings (..)
    ) where

import Prelude hiding (exp)
import Language.Haskell.TH.Syntax
import Web.PathPieces
import Data.Maybe (catMaybes)
import Control.Monad (forM)
import Data.List (foldl')
import Control.Arrow (second)
import Yesod.Routes.TH.Types
import Data.Char (toLower)

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
    , mdsHandleNestedRoute :: Maybe NestedRouteSettings
    -- ^ These settings d
    --
    -- @since TODO
    }

-- | These settings describe how to handle nested routes for the given
-- dispatch. This is not used for Subsites, but for constructions like:
--
-- @
-- /admin/ AdminR:
--     /   AdminIndexR POST GET
-- @
--
-- This construction allows us to delegate nested routes to their own
-- modules for parsing, attributes, and dispatch.
--
-- @since TODO
data NestedRouteSettings = NestedRouteSettings
    { nrsClassName :: Name
    -- ^ The class to lookup an instance for a 'ResourceParent' name.
    , nrsFunctionName :: Name
    -- ^ The function name to use to delegate the rest of the dispatch.
    , nrsTargetName :: Maybe String
    -- ^ The name of the target that we are currently generating code for.
    }

data SDC = SDC
    { clause404 :: Clause
    , extraParams :: [Exp]
    , extraCons :: [Exp]
    , envExp :: Exp
    , reqExp :: Exp
    }

-- | A simpler version of Yesod.Routes.TH.Dispatch.mkDispatchClause, based on
-- view patterns.
--
-- Since 1.4.0
mkDispatchClause :: MkDispatchSettings b site c -> [ResourceTree a] -> Q Clause
mkDispatchClause MkDispatchSettings {..} resources = do
    envName <- newName "env"
    reqName <- newName "req"
    helperName <- newName "helper"

    let envE = VarE envName
        reqE = VarE reqName
        helperE = VarE helperName

    clause404' <- mkClause404 envE reqE
    getPathInfo <- mdsGetPathInfo
    let pathInfo = getPathInfo `AppE` reqE

    let sdc = SDC
            { clause404 = clause404'
            , extraParams = []
            , extraCons = []
            , envExp = envE
            , reqExp = reqE
            }
    clauses <- mapM (go mdsHandleNestedRoute sdc) resources

    return $ Clause
        [VarP envName, VarP reqName]
        (NormalB $ helperE `AppE` pathInfo)
        [FunD helperName $ concat clauses ++ [clause404']]
  where
    handlePiece :: Piece a -> Q (Pat, Maybe Exp)
    handlePiece (Static str) = return (LitP $ StringL str, Nothing)
    handlePiece (Dynamic _) = do
        x <- newName "dyn"
        let pat = ViewP (VarE 'fromPathPiece) (conPCompat 'Just [VarP x])
        return (pat, Just $ VarE x)

    handlePieces :: [Piece a] -> Q ([Pat], [Exp])
    handlePieces = fmap (second catMaybes . unzip) . mapM handlePiece

    mkCon :: String -> [Exp] -> Exp
    mkCon name = foldl' AppE (ConE $ mkName name)

    mkPathPat :: Pat -> [Pat] -> Pat
    mkPathPat final =
        foldr addPat final
      where
        addPat x y = conPCompat '(:) [x, y]

    go :: Maybe NestedRouteSettings -> SDC -> ResourceTree a -> Q [Clause]
    go mnrs sdc (ResourceParent name _check pieces children) = do
        let targetMatch =
                case mnrs of
                    Nothing ->
                        True
                    Just nrs ->
                        case nrsTargetName nrs of
                            Nothing ->
                                True
                            Just targetName ->
                                targetName == name

        let mnrs' =
                case mnrs of
                    Nothing -> Nothing
                    Just nrs ->
                        if targetMatch
                            then Just nrs { nrsTargetName = Nothing }
                            else mnrs

        datatypeExists <- lookupTypeName name
        instanceExists <- case datatypeExists of
            Nothing ->
                pure False
            Just typeName -> do
                case mnrs of
                    Just NestedRouteSettings {..} ->
                        if targetMatch
                        then isInstance nrsClassName [ConT typeName]
                        else pure False
                    Nothing ->
                        pure False

        (pats, dyns) <- handlePieces pieces
        restName <- newName "rest"
        let restE = VarE restName
            restP = VarP restName

        helperName <- newName $ "helper" ++ name
        let helperE = VarE helperName

        case instanceExists of
            True
                | Just NestedRouteSettings {..} <- mnrs -> do
                    -- TODO: Handle extraCons here.
                    let constr = foldl' AppE (ConE (mkName name)) dyns

                    expr <- [e|fmap $(pure constr) ($(pure (VarE nrsFunctionName)) ($(pure restE), snd $(pure (reqExp sdc))))|]
                    let childClause =
                            Clause
                                [restP]
                                (NormalB expr)
                                []
                    return $ [ Clause
                        [mkPathPat restP pats]
                        (NormalB $ helperE `AppE` restE)
                        [FunD helperName [childClause]]]
            _ -> do
                let sdcEnhanced =
                        sdc
                            { extraParams = extraParams sdc ++ dyns
                            , extraCons = extraCons sdc ++ [mkCon name dyns]
                            }
                    sdc' =
                        -- If we're targeting a subroute, we don't want to
                        -- accumulate extra constructors or patterns,
                        -- unless we've got a match on the target.
                        maybe sdcEnhanced (const sdc) $ mnrs >>= nrsTargetName
                childClauses <- mapM (go mnrs' sdc') children

                case mnrs >>= nrsTargetName of
                    Just target | target /= name -> do
                        -- Don't generate clauses for a nested thing we're
                        -- not targeting.
                        pure []
                    _ ->
                        return $ [ Clause
                            [mkPathPat restP pats]
                            (NormalB $ helperE `AppE` restE)
                            [FunD helperName $ concat childClauses ++ [clause404 sdc]]]
    go mnrs SDC {..} (ResourceLeaf (Resource name pieces dispatch _ _check)) = do
        case mnrs >>= nrsTargetName of
            Just _ -> do
                -- Don't generate a clause if we're focused on a target.
                -- Other code branches will set nrsTargetName to Nothing
                -- when a match has hit, so we'll generate clauses for
                -- sub-routes of a match.
                pure []
            Nothing -> do
                (pats, dyns) <- handlePieces pieces

                (chooseMethod, finalPat) <- handleDispatch dispatch dyns

                return $ pure $ Clause
                    [mkPathPat finalPat pats]
                    (NormalB chooseMethod)
                    []
      where
        handleDispatch :: Dispatch a -> [Exp] -> Q (Exp, Pat)
        handleDispatch dispatch' dyns =
            case dispatch' of
                Methods multi methods -> do
                    (finalPat, mfinalE) <-
                        case multi of
                            Nothing -> return (conPCompat '[] [], Nothing)
                            Just _ -> do
                                multiName <- newName "multi"
                                let pat = ViewP (VarE 'fromPathMultiPiece)
                                                (conPCompat 'Just [VarP multiName])
                                return (pat, Just $ VarE multiName)

                    let dynsMulti =
                            case mfinalE of
                                Nothing -> dyns
                                Just e -> dyns ++ [e]
                        route' = foldl' AppE (ConE (mkName name)) dynsMulti
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
                    sub <- newName "sub"
                    let allDyns = extraParams ++ dyns
                    sroute <- newName "sroute"
                    let sub2 = LamE [VarP sub]
                            (foldl' (\a b -> a `AppE` b) (VarE (mkName getSub) `AppE` VarE sub) allDyns)
                    let reqExp' = setPathInfoE `AppE` VarE restPath `AppE` reqExp
                        route' = foldl' AppE (ConE (mkName name)) dyns
                        route = LamE [VarP sroute] $ foldr AppE (AppE route' $ VarE sroute) extraCons
                        exp = subDispatcherE
                            `AppE` runHandlerE
                            `AppE` sub2
                            `AppE` route
                            `AppE` envExp
                            `AppE` reqExp'
                    return (exp, VarP restPath)

    mkClause404 envE reqE = do
        handler <- mds404
        runHandler <- mdsRunHandler
        let exp = runHandler `AppE` handler `AppE` envE `AppE` ConE 'Nothing `AppE` reqE
        return $ Clause [WildP] (NormalB exp) []

defaultGetHandler :: Maybe String -> String -> Q Exp
defaultGetHandler Nothing s = return $ VarE $ mkName $ "handle" ++ s
defaultGetHandler (Just method) s = return $ VarE $ mkName $ map toLower method ++ s

conPCompat :: Name -> [Pat] -> Pat
conPCompat n pats = ConP n
#if MIN_VERSION_template_haskell(2,18,0)
                         []
#endif
                         pats
