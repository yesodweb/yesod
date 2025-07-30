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
import Data.Maybe (catMaybes, fromMaybe)
import Control.Monad
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
    clauses <- concat <$> mapM (go mdsHandleNestedRoute sdc) resources

    -- reportWarning $ show ("top-level", clauses)
    return $ Clause
        [VarP envName, VarP reqName]
        (NormalB $ helperE `AppE` pathInfo)
        [FunD helperName $ fmap snd clauses ++ [clause404']]
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

    go :: Maybe NestedRouteSettings -> SDC -> ResourceTree a -> Q [(String, Clause)]
    go mnrs sdc (ResourceParent name _check pieces children) = do
        let mtargetName = mnrs >>= nrsTargetName
        let mtargetMatch =
                fmap (name ==) mtargetName

        -- If the target name is a match, then we want to set target name
        -- to Nothing so that we start generating code in recursion.
        let mnrs' = do
                nrs <- mnrs
                if fromMaybe False mtargetMatch
                    then Just nrs { nrsTargetName = Nothing }
                    else mnrs

        instanceExists <- do
            -- Generate delegated clauses if the datatype and instance
            -- already exist.
            datatypeExists <- lookupTypeName name
            case datatypeExists of
                Nothing ->
                    pure Nothing
                Just typeName -> do
                    case mnrs of
                        Just nrs ->
                            if fromMaybe True mtargetMatch
                            then do
                                t <- isInstance (nrsClassName nrs) [ConT typeName]
                                pure $ if t
                                    then Just nrs
                                    else Nothing
                            else pure Nothing
                        Nothing ->
                            pure Nothing

        (pats, dyns) <- handlePieces pieces
        restName <- newName "rest"
        let restE = VarE restName
            restP = VarP restName

        helperName <- newName $ "helper" ++ name
        let helperE = VarE helperName

        case instanceExists of
            Just NestedRouteSettings {..} -> do
                    -- TODO: Handle extraCons here.
                    let constr = foldl' AppE (ConE (mkName name)) dyns
                    expr <- [e|fmap $(pure constr) ($(pure (VarE nrsFunctionName)) ($(pure restE), snd $(pure (reqExp sdc))))|]
                    let childClause =
                            Clause
                                [restP]
                                (NormalB expr)
                                []
                    return $ [ (,) name $ Clause
                        [mkPathPat restP pats]
                        (NormalB $ helperE `AppE` restE)
                        [FunD helperName [childClause]]]
            Nothing -> do

                let sdcEnhanced =
                        sdc
                            { extraParams = extraParams sdc ++ dyns
                            , extraCons = extraCons sdc ++ [mkCon name dyns]
                            }
                    sdc' =
                        case mnrs >>= nrsTargetName of
                            Nothing ->
                                -- In this branch, we either have no
                                -- settings, no target, or the target has
                                -- been cleared and route processing should
                                -- start. In this case, we want to provide
                                -- the enhanced version.
                                sdcEnhanced
                            Just target ->
                                if target == name
                                then
                                    -- In this case, we are exactly at the
                                    -- root node for what we are focusing
                                    -- on.
                                    sdc
                                else
                                    -- In this case, we are not yet at the
                                    -- node we are focusing on, so we should
                                    -- not accumulate.
                                    sdcEnhanced

                childClauses <- concat <$> mapM (go mnrs' sdc') children

                if fromMaybe True mtargetMatch || not (null childClauses)
                    then do
                        let fullReturn =
                                [ (,) name $ Clause
                                    [mkPathPat restP pats]
                                    (NormalB $ helperE `AppE` restE)
                                    [FunD helperName $ fmap snd childClauses ++ [clause404 sdc]]]
                            passThru =
                                return childClauses

                        case mtargetMatch of
                            Nothing ->
                                -- no match, or no matching. return full.
                                pure fullReturn
                            Just True ->
                                -- we are currently in a match.
                                passThru
                            Just False ->
                                pure fullReturn

                    else do
                        -- reportWarning $ show ("Dropping clauses for", name, childClauses)
                        -- Don't generate clauses for a nested thing we're
                        -- not targeting.
                        pure []

    go mnrs SDC {..} (ResourceLeaf (Resource name pieces dispatch _ _check)) = do
        case mnrs >>= nrsTargetName of
            Just _target -> do
                -- Don't generate a clause if we're focused on a target.
                -- Other code branches will set nrsTargetName to Nothing
                -- when a match has hit, so we'll generate clauses for
                -- sub-routes of a match.
                pure []
            Nothing -> do
                (pats, dyns) <- handlePieces pieces

                (chooseMethod, finalPat) <- handleDispatch dispatch dyns

                return $ pure $ (,) name $ Clause
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
