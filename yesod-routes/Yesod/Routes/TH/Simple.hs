{-# LANGUAGE RecordWildCards, TemplateHaskell, ViewPatterns #-}
module Yesod.Routes.TH.Simple where

import Yesod.Routes.TH
import Language.Haskell.TH.Syntax
import Web.PathPieces
import Data.Maybe (mapMaybe)
import Control.Monad (forM)
import Data.List (foldl')
import Data.ByteString (ByteString)

mkSimpleDispatchClauses :: MkDispatchSettings -> [ResourceTree a] -> Q [Clause]
mkSimpleDispatchClauses MkDispatchSettings {..} (flatten -> resources) = do
    clauses <- mapM go resources
    clause404 <- mkClause404
    return $ clauses ++ [clause404]
  where
    go (FlatResource _ name pieces dispatch) = do
        let env = VarE $ mkName "env"
            req = VarE $ mkName "req"
        gpi <- mdsGetPathInfo
        gm <- mdsMethod
        let handlePiece (_, Static str) = return (LitP $ StringL str, Nothing)
            handlePiece (_, Dynamic _) = do
                x <- newName "x"
                let pat = ViewP (VarE 'fromPathPiece) (ConP 'Just [VarP x])
                return (pat, Just x)
        pairs <- mapM handlePiece pieces
        let pats = map fst pairs
            names = mapMaybe snd pairs
        runHandler <- mdsRunHandler
        let route = foldl' AppE (ConE (mkName name)) (map VarE names)
        exp <- case dispatch of
            Methods _ [] -> error "no methods"
            Methods _ methods -> do
                matches <- forM methods $ \method -> do
                    handler' <- mdsGetHandler (Just method) name
                    let handler = foldl' AppE handler' (map VarE names)
                    let body = NormalB exp
                        jroute = ConE 'Just `AppE` route
                        exp = runHandler `AppE` handler `AppE` env `AppE` jroute `AppE` req
                    return $ Match (LitP $ StringL method) body []
                let method = SigE (gm `AppE` req) (ConT ''ByteString)
                return $ CaseE method matches
        return $ Clause
            [ VarP $ mkName "env"
            , AsP (mkName "req") (ViewP gpi (ListP pats))
            ] (NormalB exp) []

    mkClause404 = do
        handler <- mds404
        runHandler <- mdsRunHandler
        let exp = runHandler `AppE` handler `AppE` VarE (mkName "env") `AppE` ConE 'Nothing `AppE` VarE (mkName "req")
        return $ Clause
            [ VarP (mkName "env")
            , VarP (mkName "req")
            ] (NormalB exp) []
