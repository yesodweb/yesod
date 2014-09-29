{-# LANGUAGE RecordWildCards, TemplateHaskell, ViewPatterns #-}
module Yesod.Routes.TH.Dispatch
    ( MkDispatchSettings (..)
    , mkDispatchClause
    , defaultGetHandler
    ) where

import Prelude hiding (exp)
import Language.Haskell.TH.Syntax
import Web.PathPieces
import Data.Maybe (catMaybes)
import Control.Monad (forM)
import Data.List (foldl')
import Control.Arrow (second)
import System.Random (randomRIO)
import Yesod.Routes.TH.Types
import Data.Char (toLower)

data MkDispatchSettings = MkDispatchSettings
    { mdsRunHandler :: Q Exp
    , mdsSubDispatcher :: Q Exp
    , mdsGetPathInfo :: Q Exp
    , mdsSetPathInfo :: Q Exp
    , mdsMethod :: Q Exp
    , mds404 :: Q Exp
    , mds405 :: Q Exp
    , mdsGetHandler :: Maybe String -> String -> Q Exp
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
mkDispatchClause :: MkDispatchSettings -> [ResourceTree a] -> Q Clause
mkDispatchClause MkDispatchSettings {..} resources = do
    suffix <- qRunIO $ randomRIO (1000, 9999 :: Int)
    envName <- newName $ "env" ++ show suffix
    reqName <- newName $ "req" ++ show suffix
    helperName <- newName $ "helper" ++ show suffix

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
    clauses <- mapM (go sdc) resources

    return $ Clause
        [VarP envName, VarP reqName]
        (NormalB $ helperE `AppE` pathInfo)
        [FunD helperName $ clauses ++ [clause404']]
  where
    handlePiece :: Piece a -> Q (Pat, Maybe Exp)
    handlePiece (Static str) = return (LitP $ StringL str, Nothing)
    handlePiece (Dynamic _) = do
        x <- newName "dyn"
        let pat = ViewP (VarE 'fromPathPiece) (ConP 'Just [VarP x])
        return (pat, Just $ VarE x)

    handlePieces :: [Piece a] -> Q ([Pat], [Exp])
    handlePieces = fmap (second catMaybes . unzip) . mapM handlePiece

    mkCon :: String -> [Exp] -> Exp
    mkCon name = foldl' AppE (ConE $ mkName name)

    mkPathPat :: Pat -> [Pat] -> Pat
    mkPathPat final =
        foldr addPat final
      where
        addPat x y = ConP '(:) [x, y]

    go :: SDC -> ResourceTree a -> Q Clause
    go sdc (ResourceParent name _check pieces children) = do
        (pats, dyns) <- handlePieces pieces
        let sdc' = sdc
                { extraParams = extraParams sdc ++ dyns
                , extraCons = extraCons sdc ++ [mkCon name dyns]
                }
        childClauses <- mapM (go sdc') children

        restName <- newName "rest"
        let restE = VarE restName
            restP = VarP restName

        helperName <- newName $ "helper" ++ name
        let helperE = VarE helperName

        return $ Clause
            [mkPathPat restP pats]
            (NormalB $ helperE `AppE` restE)
            [FunD helperName $ childClauses ++ [clause404 sdc]]
    go SDC {..} (ResourceLeaf (Resource name pieces dispatch _ _check)) = do
        (pats, dyns) <- handlePieces pieces

        (chooseMethod, finalPat) <- handleDispatch dispatch dyns

        return $ Clause
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
                            Nothing -> return (ConP '[] [], Nothing)
                            Just _ -> do
                                multiName <- newName "multi"
                                let pat = ViewP (VarE 'fromPathMultiPiece)
                                                (ConP 'Just [VarP multiName])
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
                            let handlerE = foldl' AppE handlerE' allDyns
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
                    let sub2 = LamE [VarP sub]
                            (foldl' (\a b -> a `AppE` b) (VarE (mkName getSub) `AppE` VarE sub) dyns)
                    let reqExp' = setPathInfoE `AppE` VarE restPath `AppE` reqExp
                        route' = foldl' AppE (ConE (mkName name)) dyns
                        route = foldr AppE route' extraCons
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
