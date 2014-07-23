{-# LANGUAGE TemplateHaskell #-}
module Yesod.Routes.TH.Dispatch
    ( -- ** Dispatch
      mkDispatchClause
    , MkDispatchSettings (..)
    , defaultGetHandler
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
import Data.Text.Encoding (encodeUtf8)

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

defaultGetHandler :: Maybe String -> String -> Q Exp
defaultGetHandler Nothing s = return $ VarE $ mkName $ "handle" ++ s
defaultGetHandler (Just method) s = return $ VarE $ mkName $ map toLower method ++ s

-- |
--
-- This function will generate a single clause that will address all
-- your routing needs. It takes four arguments. The fourth (a list of
-- 'Resource's) is self-explanatory. We\'ll discuss the first
-- three. But first, let\'s cover the terminology.
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
-- request method and path pieces. This is the second argument of our function.
--
-- Finally, we need a way to decide which of the possible formats
-- should the handler send the data out. Think of each URL holding an
-- abstract object which has multiple representation (JSON, plain HTML
-- etc). Each client might have a preference on which format it wants
-- the abstract object in. For example, a javascript making a request
-- (on behalf of a browser) might prefer a JSON object over a plain
-- HTML file where as a user browsing with javascript disabled would
-- want the page in HTML. The third argument is a function that
-- converts the abstract object to the desired representation
-- depending on the preferences sent by the client.
--
-- The typical values for the first three arguments are,
-- @'yesodRunner'@ for the first, @'yesodDispatch'@ for the second and
-- @fmap 'chooseRep'@.

mkDispatchClause :: MkDispatchSettings
                 -> [ResourceTree a]
                 -> Q Clause
mkDispatchClause mds ress' = do
    -- Allocate the names to be used. Start off with the names passed to the
    -- function itself (with a 0 suffix).
    --
    -- We don't reuse names so as to avoid shadowing names (triggers warnings
    -- with -Wall). Additionally, we want to ensure that none of the code
    -- passed to toDispatch uses variables from the closure to prevent the
    -- dispatch data structure from being rebuilt on each run.
    getEnv0 <- newName "yesod_dispatch_env0"
    req0 <- newName "req0"
    pieces <- [|$(mdsGetPathInfo mds) $(return $ VarE req0)|]

    -- Name of the dispatch function
    dispatch <- newName "dispatch"

    -- Dispatch function applied to the pieces
    let dispatched = VarE dispatch `AppE` pieces

    -- The 'D.Route's used in the dispatch function
    routes <- mapM (buildRoute mds) ress

    -- The dispatch function itself
    toDispatch <- [|D.toDispatch|]
    let dispatchFun = FunD dispatch
            [Clause
                []
                (NormalB $ toDispatch `AppE` ListE routes)
                []
            ]

    -- The input to the clause.
    let pats = map VarP [getEnv0, req0]

    -- For each resource that dispatches based on methods, build up a map for handling the dispatching.
    methodMaps <- catMaybes <$> mapM (buildMethodMap mds) ress

    u <- [|case $(return dispatched) of
            Just f -> f $(return $ VarE getEnv0)
                        $(return $ VarE req0)
            Nothing -> $(mdsRunHandler mds)
                            $(mds404 mds)
                            $(return $ VarE getEnv0)
                            Nothing
                            $(return $ VarE req0)
          |]
    return $ Clause pats (NormalB u) $ dispatchFun : methodMaps
  where
    ress = flatten ress'

-- | Determine the name of the method map for a given resource name.
methodMapName :: String -> Name
methodMapName s = mkName $ "methods" ++ s

buildMethodMap :: MkDispatchSettings
               -> FlatResource a
               -> Q (Maybe Dec)
buildMethodMap _ (FlatResource _ _ _ (Methods _ []) _) = return Nothing -- single handle function
buildMethodMap mds (FlatResource parents name pieces' (Methods mmulti methods) _check) = do
    fromList <- [|Map.fromList|]
    methods' <- mapM go methods
    let exp = fromList `AppE` ListE methods'
    let fun = FunD (methodMapName name) [Clause [] (NormalB exp) []]
    return $ Just fun
  where
    pieces = concat $ map snd parents ++ [pieces']
    go method = do
        func <- mdsGetHandler mds (Just method) name
        pack' <- [|encodeUtf8 . pack|]
        let isDynamic Dynamic{} = True
            isDynamic _ = False
        let argCount = length (filter isDynamic pieces) + maybe 0 (const 1) mmulti
        xs <- replicateM argCount $ newName "arg"
        runHandler <- mdsRunHandler mds
        let rhs
                | null xs = runHandler `AppE` func
                | otherwise =
                    LamE (map VarP xs) $
                    runHandler `AppE` (foldl' AppE func $ map VarE xs)
        return $ TupE
            [ pack' `AppE` LitE (StringL method)
            , rhs
            ]
buildMethodMap _ (FlatResource _ _ _ Subsite{} _check) = return Nothing

-- | Build a single 'D.Route' expression.
buildRoute :: MkDispatchSettings -> FlatResource a -> Q Exp
buildRoute mds (FlatResource parents name resPieces resDisp _) = do
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
            mds
            parents
            name
            allPieces
            resDisp)
        |]
  where
    allPieces = concat $ map snd parents ++ [resPieces]

routeArg3 :: MkDispatchSettings
          -> [(String, [Piece a])]
          -> String -- ^ name of resource
          -> [Piece a]
          -> Dispatch a
          -> Q Exp
routeArg3 mds parents name resPieces resDisp = do
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
    caller <- buildCaller mds xrest parents name resDisp $ map snd ys ++ yrest'

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
buildCaller :: MkDispatchSettings
            -> Name -- ^ xrest
            -> [(String, [Piece a])]
            -> String -- ^ name of resource
            -> Dispatch a
            -> [Name] -- ^ ys
            -> Q Exp
buildCaller mds xrest parents name resDisp ys = do
    getEnv <- newName "yesod_dispatch_env"
    req <- newName "req"

    method <- [|$(mdsMethod mds) $(return $ VarE req)|]

    let pat = map VarP [getEnv, req]

    -- Create the route
    let route = routeFromDynamics parents name ys

    exp <-
        case resDisp of
            Methods _ ms -> do
                handler <- newName "handler"

                env <- [|$(return $ VarE getEnv) (Just $(return route))|]

                -- Run the whole thing
                runner <- [|$(return $ VarE handler)
                                $(return $ VarE getEnv)
                                (Just $(return route))
                                $(return $ VarE req)
                                |]

                let myLet handlerExp =
                        LetE [FunD handler [Clause [] (NormalB handlerExp) []]] runner

                if null ms
                    then do
                        -- Just a single handler
                        base <- mdsGetHandler mds Nothing name
                        let he = foldl' (\a b -> a `AppE` VarE b) base ys
                        runHandler <- mdsRunHandler mds
                        return $ myLet $ runHandler `AppE` he
                    else do
                        -- Individual methods
                        mf <- [|Map.lookup $(return method) $(return $ VarE $ methodMapName name)|]
                        f <- newName "f"
                        let apply = foldl' (\a b -> a `AppE` VarE b) (VarE f) ys
                        body405 <-
                            [|$(mdsRunHandler mds)
                                $(mds405 mds)
                                $(return $ VarE getEnv)
                                (Just $(return route))
                                $(return $ VarE req)
                             |]
                        return $ CaseE mf
                            [ Match (ConP 'Just [VarP f]) (NormalB $ myLet apply) []
                            , Match (ConP 'Nothing []) (NormalB body405) []
                            ]

            Subsite _ getSub -> do
                sub <- newName "sub"
                let sub2 = LamE [VarP sub]
                        (foldl' (\a b -> a `AppE` VarE b) (VarE (mkName getSub) `AppE` VarE sub) ys)
                [|$(mdsSubDispatcher mds)
                    $(mdsRunHandler mds)
                    $(return sub2)
                    $(return route)
                    $(return $ VarE getEnv)
                    ($(mdsSetPathInfo mds)
                        $(return $ VarE xrest)
                        $(return $ VarE req)
                        )
                 |]

    return $ LamE pat exp

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
