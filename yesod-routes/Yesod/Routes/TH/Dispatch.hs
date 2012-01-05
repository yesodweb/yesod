{-# LANGUAGE TemplateHaskell #-}
module Yesod.Routes.TH.Dispatch
    ( -- ** Dispatch
      mkDispatchClause
    ) where

import Prelude hiding (exp)
import Yesod.Routes.TH.Types
import Language.Haskell.TH.Syntax
import Data.Maybe (catMaybes)
import Control.Monad (forM, replicateM)
import Data.Text (pack)
import qualified Yesod.Routes.Dispatch as D
import qualified Data.Map as Map
import Data.Char (toLower)
import Web.PathPieces (PathPiece (..), PathMultiPiece (..))
import Control.Applicative ((<$>))
import Data.List (foldl')

-- |
--
-- This function will generate a single clause that will address all your
-- routing needs. It takes three arguments. The third (a list of 'Resource's)
-- is self-explanatory. We\'ll discuss the first two. But first, let\'s cover
-- the terminology.
--
-- Dispatching involves a master type and a sub type. When you dispatch to the
-- top level type, master and sub are the same. Each time to dispatch to
-- another subsite, the sub changes. This requires two changes:
--
-- * Getting the new sub value. This is handled via 'subsiteFunc'.
--
-- * Figure out a way to convert sub routes to the original master route. To
-- address this, we keep a toMaster function, and each time we dispatch to a
-- new subsite, we compose it with the constructor for that subsite.
--
-- Dispatching acts on two different components: the request method and a list
-- of path pieces. If we cannot match the path pieces, we need to return a 404
-- response. If the path pieces match, but the method is not supported, we need
-- to return a 405 response.
--
-- The final result of dispatch is going to be an application type. A simple
-- example would be the WAI Application type. However, our handler functions
-- will need more input: the master/subsite, the toMaster function, and the
-- type-safe route. Therefore, we need to have another type, the handler type,
-- and a function that turns a handler into an application, i.e.
--
-- > runHandler :: handler sub master -> master -> sub -> Route sub -> (Route sub -> Route master) -> app
--
-- This is the first argument to our function. Note that this will almost
-- certainly need to be a method of a typeclass, since it will want to behave
-- differently based on the subsite.
--
-- Note that the 404 response passed in is an application, while the 405
-- response is a handler, since the former can\'t be passed the type-safe
-- route.
--
-- In the case of a subsite, we don\'t directly deal with a handler function.
-- Instead, we redispatch to the subsite, passing on the updated sub value and
-- toMaster function, as well as any remaining, unparsed path pieces. This
-- function looks like:
--
-- > dispatcher :: master -> sub -> (Route sub -> Route master) -> app -> handler sub master -> Text -> [Text] -> app
--
-- Where the parameters mean master, sub, toMaster, 404 response, 405 response,
-- request method and path pieces.
mkDispatchClause :: Q Exp -- ^ runHandler function
                 -> Q Exp -- ^ dispatcher function
                 -> Q Exp -- ^ fixHandler function
                 -> [Resource a]
                 -> Q Clause
mkDispatchClause runHandler dispatcher fixHandler ress = do
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
    routes <- mapM (buildRoute runHandler dispatcher fixHandler) ress

    -- The dispatch function itself
    toDispatch <- [|D.toDispatch|]
    let dispatchFun = FunD dispatch [Clause [] (NormalB $ toDispatch `AppE` ListE routes) []]

    -- The input to the clause.
    let pats = map VarP [master0, sub0, toMaster0, app4040, handler4050, method0, pieces0]

    -- For each resource that dispatches based on methods, build up a map for handling the dispatching.
    methodMaps <- catMaybes <$> mapM (buildMethodMap fixHandler) ress

    u <- [|case $(return dispatched) of
            Just f -> f $(return $ VarE master0)
                        $(return $ VarE sub0)
                        $(return $ VarE toMaster0)
                        $(return $ VarE app4040)
                        $(return $ VarE handler4050)
                        $(return $ VarE method0)
            Nothing -> $(return $ VarE app4040)
          |]
    return $ Clause pats (NormalB u) $ dispatchFun : methodMaps

-- | Determine the name of the method map for a given resource name.
methodMapName :: String -> Name
methodMapName s = mkName $ "methods" ++ s

buildMethodMap :: Q Exp -- ^ fixHandler
               -> Resource a
               -> Q (Maybe Dec)
buildMethodMap _ (Resource _ _ (Methods _ [])) = return Nothing -- single handle function
buildMethodMap fixHandler (Resource name pieces (Methods mmulti methods)) = do
    fromList <- [|Map.fromList|]
    methods' <- mapM go methods
    let exp = fromList `AppE` ListE methods'
    let fun = FunD (methodMapName name) [Clause [] (NormalB exp) []]
    return $ Just fun
  where
    go method = do
        fh <- fixHandler
        let func = VarE $ mkName $ map toLower method ++ name
        pack' <- [|pack|]
        let isDynamic Dynamic{} = True
            isDynamic _ = False
        let argCount = length (filter isDynamic pieces) + maybe 0 (const 1) mmulti
        xs <- replicateM argCount $ newName "arg"
        let rhs = LamE (map VarP xs) $ fh `AppE` (foldl' AppE func $ map VarE xs)
        return $ TupE [pack' `AppE` LitE (StringL method), rhs]
buildMethodMap _ (Resource _ _ Subsite{}) = return Nothing

-- | Build a single 'D.Route' expression.
buildRoute :: Q Exp -> Q Exp -> Q Exp -> Resource a -> Q Exp
buildRoute runHandler dispatcher fixHandler (Resource name resPieces resDisp) = do
    -- First two arguments to D.Route
    routePieces <- ListE <$> mapM convertPiece resPieces
    isMulti <-
        case resDisp of
            Methods Nothing _ -> [|False|]
            _ -> [|True|]

    [|D.Route $(return routePieces) $(return isMulti) $(routeArg3 runHandler dispatcher fixHandler name resPieces resDisp)|]

routeArg3 :: Q Exp -- ^ runHandler
          -> Q Exp -- ^ dispatcher
          -> Q Exp -- ^ fixHandler
          -> String -- ^ name of resource
          -> [Piece a]
          -> Dispatch a
          -> Q Exp
routeArg3 runHandler dispatcher fixHandler name resPieces resDisp = do
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
    caller <- buildCaller runHandler dispatcher fixHandler xrest name resDisp $ map snd ys ++ yrest'

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
buildCaller :: Q Exp -- ^ runHandler
            -> Q Exp -- ^ dispatcher
            -> Q Exp -- ^ fixHandler
            -> Name -- ^ xrest
            -> String -- ^ name of resource
            -> Dispatch a
            -> [Name] -- ^ ys
            -> Q Exp
buildCaller runHandler dispatcher fixHandler xrest name resDisp ys = do
    master <- newName "master"
    sub <- newName "sub"
    toMaster <- newName "toMaster"
    app404 <- newName "_app404"
    handler405 <- newName "_handler405"
    method <- newName "_method"

    let pat = map VarP [master, sub, toMaster, app404, handler405, method]

    -- Create the route
    let route = foldl' (\a b -> a `AppE` VarE b) (ConE $ mkName name) ys

    exp <-
        case resDisp of
            Methods _ ms -> do
                handler <- newName "handler"

                -- Run the whole thing
                runner <- [|$(runHandler)
                                $(return $ VarE handler)
                                $(return $ VarE master)
                                $(return $ VarE sub)
                                (Just $(return route))
                                $(return $ VarE toMaster)|]

                let myLet handlerExp =
                        LetE [FunD handler [Clause [] (NormalB handlerExp) []]] runner

                if null ms
                    then do
                        -- Just a single handler
                        fh <- fixHandler
                        let he = fh `AppE` foldl' (\a b -> a `AppE` VarE b) (VarE $ mkName $ "handle" ++ name) ys
                        return $ myLet he
                    else do
                        -- Individual methods
                        mf <- [|Map.lookup $(return $ VarE method) $(return $ VarE $ methodMapName name)|]
                        f <- newName "f"
                        let apply = foldl' (\a b -> a `AppE` VarE b) (VarE f) ys
                        let body405 =
                                VarE handler405
                                `AppE` route
                        return $ CaseE mf
                            [ Match (ConP 'Just [VarP f]) (NormalB $ myLet apply) []
                            , Match (ConP 'Nothing []) (NormalB body405) []
                            ]

            Subsite _ getSub -> do
                let sub2 = foldl' (\a b -> a `AppE` VarE b) (VarE (mkName getSub) `AppE` VarE sub) ys
                [|$(dispatcher)
                    $(return $ VarE master)
                    $(return sub2)
                    ($(return $ VarE toMaster) . $(return route))
                    $(return $ VarE app404)
                    ($(return $ VarE handler405) . $(return route))
                    $(return $ VarE method)
                    $(return $ VarE xrest)
                 |]

    return $ LamE pat exp

-- | Convert a 'Piece' to a 'D.Piece'
convertPiece :: Piece a -> Q Exp
convertPiece (Static s) = [|D.Static (pack $(lift s))|]
convertPiece (Dynamic _) = [|D.Dynamic|]
