{-# LANGUAGE TemplateHaskell #-}
module Yesod.Routes.TH.ParseRoute
    ( -- ** ParseRoute
      mkParseRouteInstance
    ) where

import Yesod.Routes.TH.Types
import Language.Haskell.TH.Syntax
import Data.Text (pack)
import Web.PathPieces (PathPiece (..), PathMultiPiece (..))
import Yesod.Routes.Class
import qualified Yesod.Routes.Dispatch as D
import Data.List (foldl')
import Control.Applicative ((<$>))
import Data.Maybe (catMaybes)
import Control.Monad (forM)
import Control.Monad (join)

-- | Clauses for the 'parseRoute' method.
mkParseRouteClauses :: [ResourceTree a] -> Q [Clause]
mkParseRouteClauses ress' = do
    pieces <- newName "pieces0"
    dispatch <- newName "dispatch"
    query <- newName "_query"

    -- The 'D.Route's used in the dispatch function
    routes <- mapM (buildRoute query) ress

    -- The dispatch function itself
    toDispatch <- [|D.toDispatch|]
    let dispatchFun = FunD dispatch
            [Clause
                []
                (NormalB $ toDispatch `AppE` ListE routes)
                []
            ]

    join' <- [|join|]
    let body = join' `AppE` (VarE dispatch `AppE` VarE pieces)
    return $ return $ Clause
        [TupP [VarP pieces, VarP query]]
        (NormalB body)
        [dispatchFun]
  where
    ress = map noMethods $ flatten ress'
    noMethods (FlatResource a b c d e) = FlatResource a b c (noMethods' d) e
    noMethods' (Methods a _) = Methods a []
    noMethods' (Subsite a b) = Subsite a b

mkParseRouteInstance :: Type -> [ResourceTree a] -> Q Dec
mkParseRouteInstance typ ress = do
    cls <- mkParseRouteClauses ress
    return $ InstanceD [] (ConT ''ParseRoute `AppT` typ)
        [ FunD 'parseRoute cls
        ]

-- | Build a single 'D.Route' expression.
buildRoute :: Name -> FlatResource a -> Q Exp
buildRoute query (FlatResource parents name resPieces resDisp _check) = do
    -- First two arguments to D.Route
    routePieces <- ListE <$> mapM convertPiece allPieces
    isMulti <-
        case resDisp of
            Methods Nothing _ -> [|False|]
            _ -> [|True|]

    [|D.Route
        $(return routePieces)
        $(return isMulti)
        $(routeArg3
            query
            parents
            name
            allPieces
            resDisp)
        |]
  where
    allPieces = concat $ map snd parents ++ [resPieces]

routeArg3 :: Name -- ^ query string parameters
          -> [(String, [Piece a])]
          -> String -- ^ name of resource
          -> [Piece a]
          -> Dispatch a
          -> Q Exp
routeArg3 query parents name resPieces resDisp = do
    pieces <- newName "pieces"

    -- Allocate input piece variables (xs) and variables that have been
    -- converted via fromPathPiece (ys)
    xs <- forM resPieces $ \piece ->
        case piece of
            Static _ -> return Nothing
            Dynamic _ -> Just <$> newName "x"

    -- Note: the zipping with Ints is just a workaround for (apparently) a bug
    -- in GHC where the identifiers are considered to be overlapping. Using
    -- newName should avoid the problem, but it doesn't.
    ys <- forM (zip (catMaybes xs) [1..]) $ \(x, i) -> do
        y <- newName $ "y" ++ show (i :: Int)
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
    caller <- buildCaller query xrest parents name resDisp $ map snd ys ++ yrest'

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
buildCaller :: Name -- ^ query string parameters
            -> Name -- ^ xrest
            -> [(String, [Piece a])]
            -> String -- ^ name of resource
            -> Dispatch a
            -> [Name] -- ^ ys
            -> Q Exp
buildCaller query xrest parents name resDisp ys = do
    -- Create the route
    let route = routeFromDynamics parents name ys

    case resDisp of
        Methods _ _ -> [|Just $(return route)|]
        Subsite _ _ -> [|fmap $(return route) $ parseRoute ($(return $ VarE xrest), $(return $ VarE query))|]

-- | Convert a 'Piece' to a 'D.Piece'
convertPiece :: Piece a -> Q Exp
convertPiece (Static s) = [|D.Static (pack $(lift s))|]
convertPiece (Dynamic _) = [|D.Dynamic|]

routeFromDynamics :: [(String, [Piece a])] -- ^ parents
                  -> String -- ^ constructor name
                  -> [Name]
                  -> Exp
routeFromDynamics [] name ys = foldl' (\a b -> a `AppE` VarE b) (ConE $ mkName name) ys
routeFromDynamics ((parent, pieces):rest) name ys =
    foldl' (\a b -> a `AppE` b) (ConE $ mkName parent) here
  where
    (here', ys') = splitAt (length $ filter isDynamic pieces) ys
    isDynamic Dynamic{} = True
    isDynamic _ = False
    here = map VarE here' ++ [routeFromDynamics rest name ys']
