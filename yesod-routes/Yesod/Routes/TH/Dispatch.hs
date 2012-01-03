{-# LANGUAGE TemplateHaskell #-}
module Yesod.Routes.TH.Dispatch
    ( -- ** Dispatch
      mkDispatchClause
    ) where

import Yesod.Routes.TH.Types
import Language.Haskell.TH.Syntax
import Data.Maybe (maybeToList, catMaybes)
import Control.Monad (replicateM, forM)
import Data.Text (pack)
import qualified Yesod.Routes.Dispatch as D
import qualified Data.Map as Map
import Data.Char (toLower)
import Web.PathPieces (PathPiece (..), PathMultiPiece (..))
import Yesod.Routes.Class
import Data.Maybe (catMaybes)
import Control.Applicative ((<$>))
import Data.List (foldl')

mkDispatchClause :: Q Exp -- ^ runHandler function
                 -> Q Exp -- ^ dispatcher function
                 -> [Resource]
                 -> Q Clause
mkDispatchClause runHandler dispatcher ress = do
    -- Allocate the names to be used. Start off with the names passed to the
    -- function itself (with a 0 suffix).
    --
    -- We don't reuse names so as to avoid shadowing names (triggers warnings
    -- with -Wall). Additionally, we want to ensure that none of the code
    -- passed to toDispatch uses variables from the closure to prevent the
    -- dispatch data structure from being rebuilt on each run.
    master0 <- newName "master0"
    sub0 <- newName "sub0"
    toMaster0 <- newName "toMaster0"
    app4040 <- newName "app4040"
    handler4050 <- newName "handler4050"
    method0 <- newName "method0"
    pieces0 <- newName "pieces0"

    -- Name of the dispatch function
    dispatch <- newName "dispatch"

    -- Dispatch function applied to the pieces
    let dispatched = VarE dispatch `AppE` VarE pieces0

    -- The 'D.Route's used in the dispatch function
    routes <- mapM (buildRoute runHandler dispatcher) ress

    -- The dispatch function itself
    toDispatch <- [|D.toDispatch|]
    let dispatchFun = FunD dispatch [Clause [] (NormalB $ toDispatch `AppE` ListE routes) []]

    -- The input to the clause.
    let pats = map VarP [master0, sub0, toMaster0, app4040, handler4050, method0, pieces0]

    -- For each resource that dispatches based on methods, build up a map for handling the dispatching.
    methodMaps <- catMaybes <$> mapM buildMethodMap ress

    u <- [|case $(return dispatched) of
            Just f -> f $(return $ VarE master0)
                        $(return $ VarE sub0)
            Nothing -> $(return $ VarE app4040)
          |]
    return $ Clause pats (NormalB u) $ dispatchFun : methodMaps

-- | Determine the name of the method map for a given resource name.
methodMapName :: String -> Name
methodMapName s = mkName $ "methods" ++ s

buildMethodMap :: Resource -> Q (Maybe Dec)
buildMethodMap (Resource _ _ (Methods _ [])) = return Nothing -- single handle function
buildMethodMap (Resource name _ (Methods _ methods)) = do
    fromList <- [|Map.fromList|]
    methods' <- mapM go methods
    let exp = fromList `AppE` ListE methods'
    let fun = FunD (methodMapName name) [Clause [] (NormalB exp) []]
    return $ Just fun
  where
    go method = do
        let func = VarE $ mkName $ map toLower method ++ name
        pack' <- [|pack|]
        return $ TupE [pack' `AppE` LitE (StringL method), func]
buildMethodMap (Resource _ _ Subsite{}) = return Nothing

-- | Build a single 'D.Route' expression.
buildRoute :: Q Exp -> Q Exp -> Resource -> Q Exp
buildRoute runHandler dispatcher (Resource name resPieces resDisp) = do
    -- First two arguments to D.Route
    routePieces <- ListE <$> mapM convertPiece resPieces
    isMulti <-
        case resDisp of
            Methods Nothing _ -> [|False|]
            _ -> [|True|]

    [|D.Route $(return routePieces) $(return isMulti) $(routeArg3 runHandler dispatcher name resPieces resDisp)|]

routeArg3 runHandler dispatcher name resPieces resDisp = do
    pieces <- newName "pieces"

    -- Allocate input piece variables (xs) and variables that have been
    -- converted via fromPathPiece (ys)
    xs <- forM resPieces $ \piece ->
        case piece of
            Static _ -> return Nothing
            Dynamic _ -> Just <$> newName "x"

    ys <- forM (catMaybes xs) $ \x -> do
        y <- newName "y"
        return (x, y)

    -- In case we have multi pieces at the end
    xrest <- newName "xrest"
    yrest <- newName "yrest"

    -- Determine the pattern for matching the pieces
    pat <-
        case resDisp of
            Methods Nothing _ -> return $ ListP $ map (maybe WildP VarP) xs
            _ -> do
                let cons = mkName ":"
                return $ foldr (\a b -> ConP cons [maybe WildP VarP a, b]) (VarP xrest) xs

    -- Convert the xs
    fromPathPiece' <- [|fromPathPiece|]
    xstmts <- forM ys $ \(x, y) -> return $ BindS (VarP y) (fromPathPiece' `AppE` VarE x)

    -- Convert the xrest if appropriate
    (reststmts, yrest') <-
        case resDisp of
            Methods (Just _) _ -> do
                fromPathMultiPiece' <- [|fromPathMultiPiece|]
                return ([BindS (VarP yrest) (fromPathMultiPiece' `AppE` VarE xrest)], [yrest])
            _ -> return ([], [])

    -- The final expression that actually uses the values we've computed
    caller <- buildCaller runHandler dispatcher name resDisp $ map snd ys ++ yrest'

    -- Put together all the statements
    just <- [|Just|]
    let stmts = concat
            [ xstmts
            , reststmts
            , [NoBindS $ just `AppE` caller]
            ]

    errorMsg <- [|error "Invariant violated"|]
    let matches =
            [ Match pat (NormalB $ DoE stmts) []
            , Match WildP (NormalB errorMsg) []
            ]

    return $ LamE [VarP pieces] $ CaseE (VarE pieces) matches

-- | The final expression in the individual Route definitions.
buildCaller runHandler dispatcher name resDisp ys = do
    master <- newName "master"
    sub <- newName "sub"
    toMaster <- newName "toMaster"
    app404 <- newName "app404"
    handler405 <- newName "handler405"
    method <- newName "method"

    let pat = map VarP [master, sub, toMaster, app404, handler405, method]

    -- Create the route
    let route = foldl' (\a b -> a `AppE` VarE b) (ConE $ mkName name) ys

    exp <-
        case resDisp of
            Methods _ ms -> do
                handler <- newName "handler"

                -- Figure out what the handler is
                handlerExp <-
                    if null ms
                        then return $ foldl' (\a b -> a `AppE` VarE b) (VarE $ mkName $ "handle" ++ name) ys
                        else do
                            mf <- [|Map.lookup $(return $ VarE method) $(return $ VarE $ methodMapName name)|]
                            f <- newName "f"
                            let apply = foldl' (\a b -> a `AppE` VarE b) (VarE f) ys
                            return $ CaseE mf
                                [ Match (ConP 'Just [VarP f]) (NormalB apply) []
                                , Match (ConP 'Nothing []) (NormalB $ VarE handler405) []
                                ]

                -- Run the whole thing
                runner <- [|$(runHandler)
                                $(return $ VarE handler)
                                $(return $ VarE master)
                                $(return $ VarE sub)
                                $(return route)
                                $(return $ VarE toMaster)|]

                return $ LetE [FunD handler [Clause [] (NormalB handlerExp) []]] runner
            Subsite _ getSub -> do
                let sub2 = foldl' (\a b -> a `AppE` VarE b) (VarE (mkName getSub) `AppE` VarE sub) ys
                [|$(dispatcher)
                    $(return $ VarE master)
                    $(return sub2)
                    ($(return $ VarE toMaster) . $(return route))
                    $(return $ VarE app404)
                    $(return $ VarE handler405)
                    $(return $ VarE method)
                 |]

    return $ LamE pat exp

-- | Convert a 'Piece' to a 'D.Piece'
convertPiece :: Piece -> Q Exp
convertPiece (Static s) = [|D.Static $(lift s)|]
convertPiece (Dynamic _) = [|D.Dynamic|]
