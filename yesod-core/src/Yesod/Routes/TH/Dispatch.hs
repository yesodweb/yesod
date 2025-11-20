{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Yesod.Routes.TH.Dispatch
    ( MkDispatchSettings (..)
    , mkDispatchClause
    , defaultGetHandler
    , NestedRouteSettings (..)
    , SDC(..)
    , mkDispatchInstance
    , mkMDS
    , mkYesodSubDispatch
    , subTopDispatch
    ) where

import Data.Maybe
import Yesod.Routes.TH.RenderRoute
import qualified Network.Wai as W
import Yesod.Core.Content (ToTypedContent (..))
import Language.Haskell.TH hiding (cxt, instanceD)
import Language.Haskell.TH.Syntax
import Yesod.Core.Types hiding (Body)
import Yesod.Core.Class.Dispatch
import Prelude hiding (exp)
import Yesod.Routes.TH.Internal
import Language.Haskell.TH.Syntax
import Web.PathPieces
import Data.Maybe (catMaybes, fromMaybe)
import Control.Monad
import Data.List (foldl')
import Control.Arrow (second)
import Yesod.Routes.TH.Types
import Data.Char (toLower)
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Trans as Trans

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
    -- ^ These settings describe how to handle nested routes. If 'Nothing'
    -- is provided, then we generate everything flat. If 'Just' is
    -- provided, then the 'NestedRouteSettings' behavior kicks on, and
    -- matches are delegated to prior code instead of flat generation.
    --
    -- @since 1.6.28.0
    , mdsNestedRouteFallthrough :: !Bool
    -- ^ When 'True', fall through if no route matches (except in the final
    -- case). When 'False', return 404 if the current route clause fails to
    -- match.
    --
    -- @since 1.6.28.0
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
-- @since 1.6.28.0
data NestedRouteSettings = NestedRouteSettings
    { nrsClassName :: Name
    -- ^ The class to lookup an instance for a 'ResourceParent' name.
    , nrsDispatchCall
        :: Exp
        -- ^ the "rest" of the route fragments
        -> SDC
        -- ^ The SDC (containing reqExp and other things at this point)
        -> Exp
        -- ^ The parent constructor
        -> [Exp]
        -- ^ The bound variables from the parent route and the path. Used
        -- as the first argument in yesodDispatchNested.
        -> Q Exp
        -- ^ The expression we want to splice in for delegating.
    , nrsTargetName :: Maybe String
    -- ^ The name of the target that we are currently generating code for.
    , nrsWrapDispatchCall
        :: SDC
        -> Exp
        -- ^ The parent constructor
        -> Exp
        -> Q Exp
    -- ^ Wrap up the dispatch call.
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
mkDispatchClause :: MkDispatchSettings b site c -> [ResourceTree a] -> Q ([String], Clause)
mkDispatchClause mds@MkDispatchSettings {..} resources = do
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
            , extraCons = []
            , envExp = envE
            , reqExp = reqE
            }
    (childNames, clauses) <- mconcat <$> mapM (go mdsHandleNestedRoute sdc) resources

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

    go :: Maybe NestedRouteSettings -> SDC -> ResourceTree a -> Q ([String], [Clause])
    go mnrs sdc (ResourceParent name _check pieces children) = do
        -- ok so basically we want to always delegate to the child class
        -- + create instances of that nested class, if those don't exist.

        {- Just result <- childCall = result -}

        -- instead of

        {- = childCall -}

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

        instanceExists <- runMaybeT $ do
            -- Generate delegated clauses if the datatype and instance
            -- already exist.
            typeName <- MaybeT $ lookupTypeName name
            nrs <- MaybeT $ pure $ mnrs
            guard $ fromMaybe True mtargetMatch
            t <- Trans.lift $ isInstance (nrsClassName nrs) [ConT typeName]
            guard t
            pure nrs

        (pats, dyns) <- handlePieces pieces
        restName <- newName "rest"
        let restE = VarE restName
            restP = VarP restName

        helperName <- newName $ "helper" ++ name
        let helperE = VarE helperName

        case instanceExists of
            Just NestedRouteSettings {..} -> do
                    let constr = foldl' AppE (ConE (mkName name)) dyns
                    expr <- nrsDispatchCall restE sdc constr (extraParams sdc ++ dyns)
                    let childClause =
                            Clause
                                [restP]
                                (NormalB expr)
                                []
                    constrName <- newName "constr"
                    body <- mkGuardedBody (helperE `AppE` restE) $ \match -> do
                        nrsWrapDispatchCall sdc constr (VarE match)

                    pure
                        ( []
                        , [ Clause
                            [mkPathPat restP pats]
                            body
                            [FunD helperName [childClause]]]
                        )

            Nothing -> do
                -- So, in the case that we do not have an instance: we want
                -- to generate it. However, we've collapsed several bits of
                -- information:
                --
                -- 1. Does the datatype exist in scope?
                -- 2. Does it have an instance of the nested class?
                -- 3. Do we have nested route settings?
                --
                -- So we need to disentangle that information still.
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
                                    -- accumulate.
                                    sdcEnhanced

                (childNames, childClauses) <- mconcat <$> mapM (go mnrs' sdc') children

                if fromMaybe True mtargetMatch || not (null childClauses)
                    then do
                        let fullReturn =
                                [ Clause
                                    [mkPathPat restP pats]
                                    (NormalB $ helperE `AppE` restE)
                                    [FunD helperName $ childClauses ++ [clause404 sdc]]]
                            passThru =
                                return childClauses

                        fmap ((,) (name : childNames)) $ case mtargetMatch of
                            Nothing ->
                                -- no match, or no matching. return full.
                                pure fullReturn
                            Just True ->
                                -- we are currently in a match.
                                passThru
                            Just False ->
                                pure fullReturn

                    else do
                        -- Don't generate clauses for a nested thing we're
                        -- not targeting.
                        pure ([], [])

    go mnrs SDC {..} (ResourceLeaf (Resource name pieces dispatch _ _check)) = do
        case mnrs >>= nrsTargetName of
            Just _target -> do
                -- Don't generate a clause if we're focused on a target.
                -- Other code branches will set nrsTargetName to Nothing
                -- when a match has hit, so we'll generate clauses for
                -- sub-routes of a match.
                pure ([], [])
            Nothing -> do
                (pats, dyns) <- handlePieces pieces

                (chooseMethod, finalPat) <- handleDispatch dispatch dyns

                pure
                    ( []
                    , pure $ Clause
                        [mkPathPat finalPat pats]
                        (NormalB chooseMethod)
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
        case mdsHandleNestedRoute >> guard mdsNestedRouteFallthrough of
            Nothing -> do
                handler <- mds404
                runHandler <- mdsRunHandler
                let exp = runHandler `AppE` handler `AppE` envE `AppE` ConE 'Nothing `AppE` reqE
                return $ Clause [WildP] (NormalB exp) []
            Just () -> do
                return $ Clause [WildP] (NormalB (ConE 'Nothing)) []

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
    let patG =
            PatG [BindS (conPCompat 'Just [VarP matchName]) exp]
    pure $ GuardedB [(patG, result)]

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
    -> Maybe String
    -- ^ The nested subroute we're focusing on, if present.
    -> Type
    -- ^ The master site type
    -> Cxt
    -- ^ Context of the instance
    -> (Exp -> Q Exp)
    -- ^ Unwrap handler
    -> [ResourceTree Type]
    -- ^ The resource
    -> DecsQ
mkDispatchInstance routeOpts Nothing master cxt f res = do
    let mds =
            mkMDS
                f
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
            , mdsHandleNestedRoute = Just NestedRouteSettings
                { nrsClassName = ''YesodDispatchNested
                , nrsWrapDispatchCall =
                    \sdc constrExpr hndlr ->
                        [e|
                            $(mdsRunHandler mds)
                                (fst $(pure hndlr))
                                $(pure $ envExp sdc)
                                ($(pure constrExpr) <$> snd $(pure hndlr))
                                $(pure $ reqExp sdc)
                        |]
                , nrsDispatchCall =
                    \restExpr sdc constrExpr dyns -> do
                        let dynsExpr =
                                case dyns of
                                    [] -> [| () |]
                                    [a] -> pure a
                                    _ -> pure $ mkTupE dyns
                        [e|
                            yesodDispatchNested
                                $(dynsExpr)
                                ($(mdsMethod mds) $(pure $ reqExp sdc))
                                $(pure restExpr)
                            |]
                , nrsTargetName = Nothing
                }
            }
    (childNames, clause') <- mkDispatchClause mdsWithNestedDispatch res
    let thisDispatch = FunD 'yesodDispatch [clause']
    childInstances <-
        fmap mconcat $ forM childNames $ \name -> do
            mkDispatchInstance routeOpts (Just name) master cxt f res
    return (instanceD cxt yDispatch [thisDispatch] : childInstances)
  where
    yDispatch = ConT ''YesodDispatch `AppT` master

mkDispatchInstance routeOpts (Just target) master cxt f res = do
    mstuff <- findNestedRoute target res
    (prePieces, subres) <- case mstuff of
        Nothing ->
            fail "Target was not found in resources."
        Just stuff ->
            pure stuff

    let preDyns =
            mapMaybe
                (\p -> case p of
                    Static _ -> Nothing
                    Dynamic a -> Just a)
                prePieces

    parentDynNs <- forM preDyns $ \_ -> newName "parentDyn"

    parentDynsP <-
        case parentDynNs of
            [] -> [p| () |]
            [x] -> varP x
            xs -> pure $ TupP $ map VarP xs

    let addParentDynsToDispatch exp = do
            foldl' AppE exp (map VarE parentDynNs)

    let mds =
            mkMDS
                f
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
            { mdsRunHandler =
                [| \handler _env mroute _req ->
                    Just (fmap toTypedContent handler, mroute)
                 |]
            , mdsGetPathInfo =
                [| snd |]
            , mdsMethod =
                [| fst |]
            , mdsHandleNestedRoute = Just NestedRouteSettings
                { nrsClassName = ''YesodDispatchNested
                , nrsWrapDispatchCall =
                    \sdc constrExpr hndlr ->
                        [e|
                            $(mdsRunHandler mdsWithNestedDispatch)
                                (fst $(pure hndlr))
                                $(pure $ envExp sdc)
                                ($(pure constrExpr) <$> snd $(pure hndlr))
                                $(pure $ reqExp sdc)
                        |]
                , nrsDispatchCall =
                    \restExpr sdc constrExpr dyns -> do
                        let dynsExpr =
                                case map VarE parentDynNs <> dyns of
                                    [] -> [| () |]
                                    [a] -> pure a
                                    newDyns -> pure $ mkTupE newDyns
                        [e|
                            yesodDispatchNested
                                $(dynsExpr)
                                ($(mdsMethod mdsWithNestedDispatch) $(pure $ reqExp sdc))
                                $(pure restExpr)
                            |]
                , nrsTargetName = Nothing
                }
            , mdsGetHandler = \mmethod name ->
                    addParentDynsToDispatch <$>
                        mdsGetHandler mds mmethod name
            }

    parentDynT <-
        case preDyns of
            [] -> [t| () |]
            [t] -> pure t
            ts ->
                pure $ foldl' AppT (TupleT (length ts)) ts


    nestHelpN <- newName "nestHelp"
    methodN <- newName "method"
    fragmentsN <- newName "fragments"

    (childNames, clause') <- mkDispatchClause mdsWithNestedDispatch subres

    let thisDispatch = FunD 'yesodDispatchNested
            [Clause
                [parentDynsP, VarP methodN, VarP fragmentsN]
                (NormalB $
                    VarE nestHelpN
                    `AppE` ConE '()
                    -- The `mkDispatchClause` expects to take two
                    -- arguments: env and req. But we can determine what
                    -- those are and mean through the MDS. We do not need
                    -- them to be the yesod site or the actual request.
                    `AppE` mkTupE [VarE methodN, VarE fragmentsN]
                )
                [FunD nestHelpN [clause']]
            ]
    let targetT = conT (mkName target)
    yDispatchNested <- [t| YesodDispatchNested $(targetT) |]

    childInstances <-
        fmap mconcat $ forM childNames $ \name -> do
            mkDispatchInstance routeOpts (Just name) master cxt f res
    parentSiteT <- [t| ParentSite $(targetT) |]
    parentDynSig <- [t| ParentArgs $(targetT) |]
    return
        ( instanceD cxt yDispatchNested
            [ TySynInstD $ TySynEqn Nothing parentSiteT master
            , TySynInstD $ TySynEqn Nothing parentDynSig parentDynT
            , thisDispatch
            ]
        : childInstances
        )

-- | Given a target 'String', find the 'ResourceParent' in the
-- @['ResourceTree' a]@ corresponding to that target and return it.
-- Also return the @['Piece' a]@ captures that precede it.
findNestedRoute :: String -> [ResourceTree a] -> Q (Maybe ([Piece a], [ResourceTree a]))
findNestedRoute _ [] = pure Nothing
findNestedRoute target (res : ress) =
    case res of
        ResourceLeaf _ -> do
            findNestedRoute target ress
        ResourceParent name _overlap pieces children -> do
            if name == target
                then pure $ Just (pieces, children)
                else do
                    mresult <- findNestedRoute target children
                    case mresult of
                        Nothing -> do
                            findNestedRoute target ress
                        Just (typs, childRoute) -> do
                            pure $ Just (pieces <> typs, childRoute)

mkYesodSubDispatch :: [ResourceTree a] -> Q Exp
mkYesodSubDispatch res = do
    (childNames, clause') <-
        mkDispatchClause
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
mkMDS f rh sd = MkDispatchSettings
    { mdsRunHandler = rh
    , mdsSubDispatcher = sd
    , mdsGetPathInfo = [|W.pathInfo|]
    , mdsSetPathInfo = [|\p r -> r { W.pathInfo = p }|]
    , mdsMethod = [|W.requestMethod|]
    , mds404 = [|void notFound|]
    , mds405 = [|void badMethod|]
    , mdsGetHandler = defaultGetHandler
    , mdsUnwrapper = f
    , mdsHandleNestedRoute = Nothing
    , mdsNestedRouteFallthrough = False
    }
