{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Yesod.Routes.TH.Dispatch
    ( MkDispatchSettings (..)
    , mkDispatchClause
    , defaultGetHandler
    , NestedRouteSettings (..)
    , DispatchPhase (..)
    , SDC(..)
    , mkDispatchInstance
    , mkNestedDispatchInstance
    , mkMDS
    , mkYesodSubDispatch
    , subTopDispatch
    ) where

import Data.Maybe
import Data.Proxy (Proxy(..))
import Yesod.Routes.TH.RenderRoute
import qualified Network.Wai as W
import Yesod.Core.Content (ToTypedContent (..))
import Language.Haskell.TH hiding (cxt, instanceD)
import Yesod.Core.Types hiding (Body)
import Yesod.Core.Class.Dispatch
import Prelude hiding (exp)
import Yesod.Routes.TH.Internal
import Language.Haskell.TH.Syntax
import Web.PathPieces
import Control.Monad
import Data.Functor (void)
import Data.List (foldl')
import Control.Arrow (second)
import Yesod.Routes.TH.Types
import Data.Char (toLower)
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Trans as Trans
import Yesod.Core.Internal.Run
import Yesod.Core.Handler
import Yesod.Routes.TH.RenderRoute (nullifyWhenNoParam)

-- | This datatype describes how to create the dispatch clause for a route
-- path.
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
    , mdsHandleNestedRoute :: NestedRouteSettings
    -- ^ These settings describe how to handle nested routes.
    --
    -- @since 1.6.28.0
    , mdsNestedRouteFallthrough :: !Bool
    -- ^ When 'True', fall through if no route matches (except in the final
    -- case). When 'False', return 404 if the current route clause fails to
    -- match.
    --
    -- @since 1.6.28.0
    }

data DispatchPhase = TopLevelDispatch | NestedDispatch

-- | This function determines the phase at the call site of
-- 'mkDispatchInstance' by reflecting on the 'nrsTargetName'. If this value
-- is present, then we are doing a nested dispatch. Otherwise, we are doing
-- a top-level dispatch.
determinePhase :: MkDispatchSettings x y z -> DispatchPhase
determinePhase mds =
    case nrsTargetName (mdsHandleNestedRoute mds) of
        Nothing ->
            TopLevelDispatch
        Just _ ->
            NestedDispatch

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
-- @since 1.6.28.0
data NestedRouteSettings = NestedRouteSettings
    { nrsTargetName :: Maybe String
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
-- The function returns the 'Clause' for the dispatch, along with
-- a @['Name']@ corresponding to the names of types that require generating
-- instances for delegation classes. For 'ParseRoute', that's
-- 'ParseRouteNtested'. For 'YesodDispatch', that's 'YesodDispatchNested'.
-- Since 1.4.0
mkDispatchClause :: forall a b site c. DispatchPhase -> [Name] -> [Exp] -> [(Type, Name)] -> MkDispatchSettings b site c -> [ResourceTree a] -> Q ([String], Clause)
mkDispatchClause phase preDyns parentCons tyargs mds@MkDispatchSettings {..} resources = do
    envName <- newName "env"
    reqName <- newName "req"
    helperName <- newName "dispatchHelper"

    let envE = VarE envName
        reqE = VarE reqName
        helperE = VarE helperName

    clause404' <- mkClause404 envE reqE
    getPathInfo <- mdsGetPathInfo
    let pathInfo = getPathInfo `AppE` reqE

    let sdc = SDC
            { clause404 = clause404'
            , extraParams = []
            , extraCons = parentCons
            , envExp = envE
            , reqExp = reqE
            }
    (childNames, clauses) <- mconcat <$> mapM (go phase mdsHandleNestedRoute sdc) resources

    pure
        ( childNames
        , Clause
            [VarP envName, VarP reqName]
            (NormalB $ helperE `AppE` pathInfo)
            [FunD helperName $ clauses ++ [clause404']]
        )
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

    go :: DispatchPhase -> NestedRouteSettings -> SDC -> ResourceTree a -> Q ([String], [Clause])
    go _phase nrs sdc (ResourceParent name _check pieces _children) = do
        let mtargetName = nrsTargetName nrs
        let mtargetMatch =
                fmap (name ==) mtargetName

        instanceExists <- runMaybeT $ do
            -- Generate delegated clauses if the datatype and instance
            -- already exist.
            typeName <- MaybeT $ lookupTypeName name
            guard $ fromMaybe True mtargetMatch
            t <- Trans.lift $ isInstance ''YesodDispatchNested [ConT typeName]
            guard t

        (pats, dyns) <- handlePieces pieces
        restName <- newName "rest"
        let restE = VarE restName
            restP = VarP restName

        helperName <- newName $ "helper" ++ name
        let helperE = VarE helperName

        let constr = foldl' AppE (ConE (mkName name)) dyns
            routeDyns = dyns
            -- ParentArgs for the nested route are the dynamics from THIS route's pieces
            -- plus any accumulated parent dynamics from outer scopes
            thisRouteParentArgs = fmap VarE preDyns <> extraParams sdc ++ dyns
        expr <- nestedDispatchCall name routeDyns tyargs mds restE sdc thisRouteParentArgs
        let childClause =
                Clause
                    [restP]
                    (NormalB expr)
                    []

        body <- if mdsNestedRouteFallthrough
            then mkGuardedBody (helperE `AppE` restE) $ \match' -> do
                wrapNestedDispatchCall phase mds sdc constr (VarE match')
            else do
                -- When fallthrough is disabled, explicitly handle Nothing with 404
                matchName <- newName "match"
                wrapped <- wrapNestedDispatchCall phase mds sdc constr (VarE matchName)
                handler <- mds404
                runHandlerE <- mdsRunHandler
                let notFoundExp = runHandlerE `AppE` handler `AppE` envExp sdc `AppE` ConE 'Nothing `AppE` reqExp sdc
                return $ NormalB $ CaseE (helperE `AppE` restE)
                    [ Match (conPCompat 'Just [VarP matchName]) (NormalB wrapped) []
                    , Match (conPCompat 'Nothing []) (NormalB notFoundExp) []
                    ]

        pure
            ( if isJust instanceExists then mempty else [name]
            , [ Clause
                [mkPathPat restP pats]
                body
                [FunD helperName [childClause]]]
            )

    go phase nrs SDC {..} (ResourceLeaf (Resource name pieces dispatch _ _check)) = do
        case pure nrs >>= nrsTargetName of
            Just _target -> do
                -- Don't generate a clause if we're focused on a target.
                -- Other code branches will set nrsTargetName to Nothing
                -- when a match has hit, so we'll generate clauses for
                -- sub-routes of a match.
                pure ([], [])
            Nothing -> do
                (pats, dyns) <- handlePieces pieces

                (chooseMethod, finalPat) <- handleDispatch dispatch dyns

                -- For nested dispatch, wrap the result in Just
                -- chooseMethod already has type (Response -> IO ResponseReceived) -> IO ResponseReceived
                -- We just wrap it in Just, no need for extra lambda
                wrappedMethod <- case phase of
                    NestedDispatch -> return $ ConE 'Just `AppE` chooseMethod
                    TopLevelDispatch -> return chooseMethod

                pure
                    ( []
                    , pure $ Clause
                        [mkPathPat finalPat pats]
                        (NormalB wrappedMethod)
                        []
                    )
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
        let actual404 = do
                handler <- mds404
                runHandlerE <- mdsRunHandler
                let baseExp = runHandlerE `AppE` handler `AppE` envE `AppE` ConE 'Nothing `AppE` reqE
                    -- In NestedDispatch mode, wrap the result in Just
                    exp = case phase of
                        TopLevelDispatch -> baseExp
                        NestedDispatch -> ConE 'Just `AppE` baseExp
                return $ Clause [WildP] (NormalB exp) []
            fallthrough404 = do
                return $ Clause [WildP] (NormalB (ConE 'Nothing)) []

        case guard mdsNestedRouteFallthrough of
            Nothing -> actual404
            Just () ->
                case phase of
                    TopLevelDispatch -> actual404
                    NestedDispatch -> fallthrough404

-- | This function generates code to call 'yesodDispatchNested'.
nestedDispatchCall
    :: String
    -- ^ The name of the nested route (e.g., "FirstFooR")
    -> [Exp]
    -- ^ The dynamic arguments for this route constructor
    -> [(Type, Name)]
    -- ^ Type arguments for parameterized routes
    -> MkDispatchSettings w x z
    -- ^ The dispatch settings (not used in new signature)
    -> Exp
    -- ^ The @restExpr@ representing the remainder of the path pieces (not used in new signature)
    -> SDC
    -- ^ The accumulated 'SDC'.
    -> [Exp]
    -- ^ The parent dynamic bound variables (for passing as ParentArgs).
    -> Q Exp
nestedDispatchCall routeName routeDyns tyargs _mds _restExpr sdc parentDyns = do
    childName <- newName "child"
    let parentDynsExpr =
            case parentDyns of
                [] -> [| () |]
                [a] -> pure a
                _ -> pure $ mkTupE parentDyns
        -- Generate a properly-typed Proxy to help type inference
        -- Apply type arguments for parameterized routes
        routeType = foldl' (\t (ty, _) -> t `AppT` ty) (ConT (mkName routeName)) tyargs
        proxyExpr = SigE (ConE 'Proxy) (AppT (ConT ''Proxy) routeType)
        -- Build the wrapper function
        -- The wrapper should apply the route constructor with its dynamics, then any accumulated parent constructors
        -- If routeDyns = [d1, d2] and extraCons = [ParentCon], we want:
        --   \child -> ParentCon (RouteCon d1 d2 child)
        -- If extraCons = [], we want:
        --   \child -> RouteCon d1 d2 child
        routeConApp = foldl' AppE (ConE (mkName routeName)) routeDyns `AppE` VarE childName
        wrapperBody = foldr AppE routeConApp (extraCons sdc)
        wrapperExpr = LamE [VarP childName] wrapperBody
    [e|
        yesodDispatchNested
            $(pure proxyExpr)
            $(parentDynsExpr)
            $(pure wrapperExpr)
            $(pure (envExp sdc))
            $(pure $ reqExp sdc)
        |]

-- | Wrap the call to 'yesodDispatchNested' in a manner appropriate to the
-- 'DispatchPhase' in the 'MkDispatchSettings'.
--
-- With the new signature, 'yesodDispatchNested' returns:
--   Maybe ((Response -> IO ResponseReceived) -> IO ResponseReceived)
--
-- For TopLevelDispatch, we need to convert this to an Application by
-- providing it with the respond callback.
--
-- For NestedDispatch, we just pass it through (it's already the right type).
wrapNestedDispatchCall
    :: DispatchPhase
    -> MkDispatchSettings x y z
    -- ^ Dispatch settings (unused in new implementation)
    -> SDC
    -- ^ Accumulated route pieces (unused in new implementation)
    -> Exp
    -- ^ The parent constructor expr for the route datatype (unused in new implementation)
    -> Exp
    -- ^ The call to 'yesodDispatchNested'
    -> Q Exp
wrapNestedDispatchCall dispatchPhase _mds _sdc _constrExpr hndlr =
    case dispatchPhase of
        TopLevelDispatch ->
            -- mkGuardedBody already unwrapped the Maybe, so just return the handler
            pure hndlr
        NestedDispatch ->
            -- In nested dispatch, return the handler as-is (will be wrapped in Maybe at leaf level)
            pure hndlr

-- | Given an 'Exp' which should result in a @'Maybe' a@, does:
--
-- @
--   | Just a <- exp = mkRhs a
-- @
--
mkGuardedBody
    :: Exp
    -- ^ The expression to match Just with
    -> (Name -> Q Exp)
    -- ^ The function to take a 'Name' and create the right hand value on
    -- successful match.
    -> Q Body
mkGuardedBody exp mkRhs = do
    matchName <- newName "match"
    result <- mkRhs matchName
    let patGuard =
            PatG [BindS (conPCompat 'Just [VarP matchName]) exp]
    pure $ GuardedB [(patGuard, result)]

defaultGetHandler :: Maybe String -> String -> Q Exp
defaultGetHandler Nothing s = return $ VarE $ mkName $ "handle" ++ s
defaultGetHandler (Just method) s = return $ VarE $ mkName $ map toLower method ++ s

-- | If the generation of @'YesodDispatch'@ instance require finer
-- control of the types, contexts etc. using this combinator. You will
-- hardly need this generality. However, in certain situations, like
-- when writing library/plugin for yesod, this combinator becomes
-- handy.
mkDispatchInstance
    :: RouteOpts
    -> Type
    -- ^ The master site type
    -> Cxt
    -- ^ Context of the instance
    -> [(Type, Name)]
    -- ^ type arguments to constructors
    -> (Exp -> Q Exp)
    -- ^ Unwrap handler
    -> [ResourceTree Type]
    -- ^ The resource
    -> DecsQ
mkDispatchInstance routeOpts@(roFocusOnNestedRoute -> Nothing) master cxt (nullifyWhenNoParam routeOpts -> tyargs) unwrapper res = do
    let mds =
            mkMDS
                unwrapper
                [|yesodRunner|]
                [|\parentRunner getSub toParent env -> yesodSubDispatch
                    YesodSubRunnerEnv
                    { ysreParentRunner = parentRunner
                    , ysreGetSub = getSub
                    , ysreToParentRoute = toParent
                    , ysreParentEnv = env
                    }
                |]
        mdsWithNestedDispatch = mds
            { mdsNestedRouteFallthrough = roNestedRouteFallthrough routeOpts
            , mdsHandleNestedRoute = NestedRouteSettings
                { nrsTargetName = Nothing
                }
            }
        phase = determinePhase mds
    (childNames, clause') <- mkDispatchClause phase [] [] tyargs mdsWithNestedDispatch res
    let thisDispatch = FunD 'yesodDispatch [clause']
    childInstances <-
        fmap mconcat $ forM childNames $ \name -> do
            mkNestedDispatchInstance routeOpts name master cxt tyargs unwrapper res
    return (instanceD cxt yDispatch [thisDispatch] : childInstances)
  where
    yDispatch = ConT ''YesodDispatch `AppT` master

mkDispatchInstance routeOpts@(roFocusOnNestedRoute -> Just target) master cxt (nullifyWhenNoParam routeOpts -> tyargs) unwrapper res = do
    mkNestedDispatchInstance routeOpts target master cxt tyargs unwrapper res

mkNestedDispatchInstance
    :: RouteOpts
    -> String
    -> Type
    -> Cxt
    -> [(Type, Name)] -- ^ tyargs
    -> (Exp -> Q Exp)
    -> [ResourceTree Type]
    -> Q [Dec]
mkNestedDispatchInstance routeOpts target master cxt (nullifyWhenNoParam routeOpts -> tyargs) unwrapper res = do
    let mstuff = findNestedRoute target res
    (prePieces, subres) <- case mstuff of
        Nothing ->
            fail "Target was not found in resources."
        Just stuff ->
            pure stuff

    -- Calculate the static parent depth (number of path pieces consumed by parent)
    let parentDepth = length prePieces
        preDyns =
            mapMaybe
                (\p -> case p of
                    Static _ -> Nothing
                    Dynamic a -> Just a)
                prePieces

    -- Generate parent dynamic argument pattern
    parentDynsP <-
        case preDyns of
            [] -> [p| () |]
            [x] -> do
                n <- newName "parentDyn"
                pure $ VarP n
            xs -> do
                ns <- forM xs $ \_ -> newName "parentDyn"
                pure $ TupP $ map VarP ns

    -- Generate names for the instance parameters
    toParentN <- newName "toParentRoute"
    yreN <- newName "yre"
    reqN <- newName "req"

    -- Generate dispatch clauses for each child resource
    clauses <- genNestedDispatchClauses
        routeOpts
        parentDepth
        parentDynsP
        (VarE toParentN)
        (VarE yreN)
        (VarE reqN)
        unwrapper
        tyargs
        subres

    let targetT = foldl' (\t x -> t `AppT` fst x) (ConT (mkName target)) tyargs
    yDispatchNested <- [t| YesodDispatchNested $(pure targetT) |]

    let pathInfoExp = VarE 'W.pathInfo `AppE` VarE reqN
        dropExp = VarE 'drop `AppE` LitE (IntegerL $ fromIntegral parentDepth) `AppE` pathInfoExp
        thisDispatch = FunD 'yesodDispatchNested
            [Clause
                [WildP, parentDynsP, VarP toParentN, VarP yreN, VarP reqN]
                (NormalB $ CaseE dropExp clauses)
                []
            ]

    childInstances <-
        fmap mconcat $ forM subres $ \childRes -> do
            case childRes of
                ResourceParent name _ _ _ -> do
                    instanceExists <- fmap (fromMaybe False) . runMaybeT $ do
                        tyname <- MaybeT $ lookupTypeName name
                        let nameAppliedT =
                                 foldl' (\t x -> t `AppT` fst x) (ConT tyname) tyargs
                        Trans.lift $ isInstance ''YesodDispatchNested [nameAppliedT]
                    if instanceExists
                        then pure []
                        else mkNestedDispatchInstance routeOpts name master cxt tyargs unwrapper res
                _ -> pure []

    return
        ( instanceD cxt yDispatchNested
            [ thisDispatch
            ]
        : childInstances
        )

-- | Generate dispatch clauses for YesodDispatchNested instances
genNestedDispatchClauses
    :: RouteOpts
    -> Int -- ^ parent piece count (for error messages/debugging)
    -> Pat -- ^ parent dynamic args pattern
    -> Exp -- ^ toParentRoute expression
    -> Exp -- ^ yre expression
    -> Exp -- ^ req expression
    -> (Exp -> Q Exp) -- ^ unwrapper
    -> [(Type, Name)] -- ^ type arguments for parameterized routes
    -> [ResourceTree Type]
    -> Q [Match]
genNestedDispatchClauses routeOpts _parentDepth parentDynsP toParentE yreE reqE unwrapper tyargs resources = do
    resourceClauses <- forM resources $ \res -> genClauseForResource res

    -- Add fallback clause
    fallbackClause <- if roNestedRouteFallthrough routeOpts
        then return $ Match WildP (NormalB $ ConE 'Nothing) []
        else do
            let notFoundExp = ConE 'Just `AppE`
                    (VarE 'yesodRunner
                        `AppE` (AppE (VarE 'void) (VarE 'notFound))
                        `AppE` yreE
                        `AppE` ConE 'Nothing
                        `AppE` reqE)
            return $ Match WildP (NormalB notFoundExp) []

    return $ concat resourceClauses ++ [fallbackClause]
  where
    -- Extract parent dynamic variables from the pattern
    getParentDynVars :: Pat -> [Name]
    getParentDynVars (VarP n) = [n]
    getParentDynVars (TupP ps) = concatMap getParentDynVars ps
    getParentDynVars _ = []

    parentDynVars = getParentDynVars parentDynsP

    genClauseForResource :: ResourceTree Type -> Q [Match]
    genClauseForResource (ResourceLeaf (Resource name pieces dispatch _ _check)) = do
        (pats, dynVars) <- genPiecePats pieces
        let routeCon = foldl' AppE (ConE (mkName name)) (map VarE dynVars)
            routeExp = toParentE `AppE` routeCon
            allDynVars = parentDynVars ++ dynVars

        case dispatch of
            Methods mmulti methods -> do
                handlerExp <- genHandlerCase name methods mmulti allDynVars
                let body = ConE 'Just `AppE`
                        (VarE 'yesodRunner
                            `AppE` handlerExp
                            `AppE` yreE
                            `AppE` (ConE 'Just `AppE` routeExp)
                            `AppE` reqE)
                return [Match (foldr consPat (conPCompat '[] []) pats) (NormalB body) []]

            Subsite _ getSub -> do
                restPath <- newName "restPath"
                sub <- newName "sub"
                let sub2 = LamE [VarP sub]
                        (foldl' (\a b -> a `AppE` b) (VarE (mkName getSub) `AppE` VarE sub) (map VarE allDynVars))
                    routeLam = LamE [VarP (mkName "sroute")] $ toParentE `AppE` (routeCon `AppE` VarE (mkName "sroute"))
                    reqExp' = (LamE [VarP (mkName "p"), VarP (mkName "r")]
                                (RecUpdE (VarE (mkName "r")) [(mkName "pathInfo", VarE (mkName "p"))]))
                            `AppE` VarE restPath
                            `AppE` reqE
                    subsiteExp = VarE 'yesodSubDispatch
                        `AppE` (RecConE 'YesodSubRunnerEnv
                            [ ('ysreParentRunner, VarE 'yesodRunner)
                            , ('ysreGetSub, sub2)
                            , ('ysreToParentRoute, routeLam)
                            , ('ysreParentEnv, yreE)
                            ])
                        `AppE` reqExp'
                return [Match (foldr consPat (VarP restPath) pats) (NormalB $ ConE 'Just `AppE` subsiteExp) []]

    genClauseForResource (ResourceParent name _check pieces _children) = do
        (pats, dynVars) <- genPiecePats pieces

        -- Build the parent args tuple for the nested call
        let allDynVars = parentDynVars ++ dynVars
            parentArgsExp = case allDynVars of
                [] -> ConE '()
                [v] -> VarE v
                vs -> mkTupE (map VarE vs)

            routeConWrapper = foldl' (\acc dv -> acc `AppE` VarE dv) (ConE (mkName name)) dynVars
            toParentComposed = InfixE (Just toParentE) (VarE '(.)) (Just routeConWrapper)

            -- Apply type arguments to the route type for parameterized routes
            routeType = foldl' (\t (ty, _) -> t `AppT` ty) (ConT (mkName name)) tyargs
            proxyExp = SigE (ConE 'Proxy) (AppT (ConT ''Proxy) routeType)

        resultName <- newName "k"

        let nestedCall = VarE 'yesodDispatchNested
                `AppE` proxyExp
                `AppE` parentArgsExp
                `AppE` toParentComposed
                `AppE` yreE
                `AppE` reqE

        if roNestedRouteFallthrough routeOpts
            then do
                -- Generate pattern guard version
                let patGuard = BindS (conPCompat 'Just [VarP resultName]) nestedCall
                    guardedBody = GuardedB [(PatG [patGuard], ConE 'Just `AppE` VarE resultName)]
                return [Match (foldr consPat WildP pats) guardedBody []]
            else do
                -- Direct call version
                return [Match (foldr consPat WildP pats) (NormalB nestedCall) []]

    genPiecePats :: [Piece Type] -> Q ([Pat], [Name])
    genPiecePats pieces = do
        results <- forM pieces $ \piece -> case piece of
            Static str -> return (LitP (StringL str), Nothing)
            Dynamic _ -> do
                n <- newName "dyn"
                let pat = ViewP (VarE 'fromPathPiece) (conPCompat 'Just [VarP n])
                return (pat, Just n)
        return (map fst results, catMaybes $ map snd results)

    consPat :: Pat -> Pat -> Pat
    consPat x y = conPCompat '(:) [x, y]

    genHandlerCase :: String -> [String] -> Maybe Type -> [Name] -> Q Exp
    genHandlerCase name methods _mmulti allDynVars = do
        let getHandlerName mmethod =
                let prefix = case mmethod of
                        Nothing -> "handle"
                        Just m -> map toLower m
                in mkName (prefix ++ name)

        if null methods
            then do
                -- No specific methods, just call handler
                let handlerName = getHandlerName Nothing
                handlerE <- unwrapper $ foldl' AppE (VarE handlerName) (map VarE allDynVars)
                return handlerE
            else do
                -- Generate method case
                methodMatches <- forM methods $ \method -> do
                    let handlerName = getHandlerName (Just method)
                    handlerE <- unwrapper $ foldl' AppE (VarE handlerName) (map VarE allDynVars)
                    return $ Match (LitP $ StringL method) (NormalB handlerE) []

                -- Add badMethod case
                badMethodHandler <- unwrapper (VarE 'badMethod)
                let badMethodMatch = Match WildP (NormalB badMethodHandler) []
                    methodCase = CaseE (VarE 'W.requestMethod `AppE` reqE) (methodMatches ++ [badMethodMatch])

                return methodCase

mkYesodSubDispatch :: [ResourceTree a] -> Q Exp
mkYesodSubDispatch res = do
    (_childNames, clause') <-
        mkDispatchClause
            TopLevelDispatch
            []
            []
            []
            (mkMDS
                return
                [|subHelper|]
                [|subTopDispatch|])
            res
    inner <- newName "inner"
    let innerFun = FunD inner [clause']
    helper <- newName "helper"
    let fun = FunD helper
                [ Clause
                    []
                    (NormalB $ VarE inner)
                    [innerFun]
                ]
    return $ LetE [fun] (VarE helper)


subTopDispatch ::
    (YesodSubDispatch sub master) =>
        (forall content. ToTypedContent content =>
            SubHandlerFor child master content ->
            YesodSubRunnerEnv child master ->
            Maybe (Route child) ->
            W.Application
        ) ->
        (mid -> sub) ->
        (Route sub -> Route mid) ->
        YesodSubRunnerEnv mid master ->
        W.Application
subTopDispatch _ getSub toParent env = yesodSubDispatch
            (YesodSubRunnerEnv
            { ysreParentRunner = ysreParentRunner env
            , ysreGetSub = getSub . ysreGetSub env
            , ysreToParentRoute = ysreToParentRoute env . toParent
            , ysreParentEnv = ysreParentEnv env
            })

mkMDS :: (Exp -> Q Exp) -> Q Exp -> Q Exp -> MkDispatchSettings a site b
mkMDS unwrapper runHandlerE subDispatcher = MkDispatchSettings
    { mdsRunHandler = runHandlerE
    , mdsSubDispatcher = subDispatcher
    , mdsGetPathInfo = [|W.pathInfo|]
    , mdsSetPathInfo = [|\p r -> r { W.pathInfo = p }|]
    , mdsMethod = [|W.requestMethod|]
    , mds404 = [|void notFound|]
    , mds405 = [|void badMethod|]
    , mdsGetHandler = defaultGetHandler
    , mdsUnwrapper = unwrapper
    , mdsHandleNestedRoute = NestedRouteSettings
        { nrsTargetName = Nothing
        }
    , mdsNestedRouteFallthrough = False
    }
