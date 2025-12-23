{-# LANGUAGE RankNTypes #-}
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
import Yesod.Core.Internal.Run
import Yesod.Core.Handler

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
mkDispatchClause :: DispatchPhase -> MkDispatchSettings b site c -> [ResourceTree a] -> Q ([String], Clause)
mkDispatchClause phase mds@MkDispatchSettings {..} resources = do
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
    go phase nrs sdc (ResourceParent name _check pieces children) = do
        -- ok so basically we want to always delegate to the child class
        -- + create instances of that nested class, if those don't exist.

        {- Just result <- childCall = result -}

        -- instead of

        {- = childCall -}

        let mtargetName = nrsTargetName nrs
        let mtargetMatch =
                fmap (name ==) mtargetName

        -- If the target name is a match, then we want to set target name
        -- to Nothing so that we start generating code in recursion.
        let mnrs' = do
                if fromMaybe False mtargetMatch
                    then Just nrs { nrsTargetName = Nothing }
                    else Nothing

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
        expr <- nestedDispatchCall mds restE sdc (extraParams sdc ++ dyns)
        let childClause =
                Clause
                    [restP]
                    (NormalB expr)
                    []
        constrName <- newName "constr"
        body <- mkGuardedBody (helperE `AppE` restE) $ \match -> do
            wrapNestedDispatchCall phase mds sdc constr (VarE match)
            -- nrsWrapDispatchCall sdc constr (VarE match)

        pure
            ( []
            , [ Clause
                [mkPathPat restP pats]
                body
                [FunD helperName [childClause]]]
            )
        -- case instanceExists of
        --     Just NestedRouteSettings {..} -> do
        --             let constr = foldl' AppE (ConE (mkName name)) dyns
        --             expr <- nestedDispatchCall mds restE sdc (extraParams sdc ++ dyns)
        --             let childClause =
        --                     Clause
        --                         [restP]
        --                         (NormalB expr)
        --                         []
        --             constrName <- newName "constr"
        --             body <- mkGuardedBody (helperE `AppE` restE) $ \match -> do
        --                 nrsWrapDispatchCall sdc constr (VarE match)

        --             pure
        --                 ( []
        --                 , [ Clause
        --                     [mkPathPat restP pats]
        --                     body
        --                     [FunD helperName [childClause]]]
        --                 )

        --     Nothing -> do
        --         -- So, in the case that we do not have an instance: we want
        --         -- to generate it. However, we've collapsed several bits of
        --         -- information:
        --         --
        --         -- 1. Does the datatype exist in scope?
        --         -- 2. Does it have an instance of the nested class?
        --         -- 3. Do we have nested route settings?
        --         --
        --         -- So we need to disentangle that information still.
        --         let sdcEnhanced =
        --                 sdc
        --                     { extraParams = extraParams sdc ++ dyns
        --                     , extraCons = extraCons sdc ++ [mkCon name dyns]
        --                     }
        --             sdc' =
        --                 case mnrs >>= nrsTargetName of
        --                     Nothing ->
        --                         -- In this branch, we either have no
        --                         -- settings, no target, or the target has
        --                         -- been cleared and route processing should
        --                         -- start. In this case, we want to provide
        --                         -- the enhanced version.
        --                         sdcEnhanced
        --                     Just target ->
        --                         if target == name
        --                         then
        --                             -- In this case, we are exactly at the
        --                             -- root node for what we are focusing
        --                             -- on.
        --                             sdc
        --                         else
        --                             -- In this case, we are not yet at the
        --                             -- node we are focusing on, so we should
        --                             -- accumulate.
        --                             sdcEnhanced

        --         (childNames, childClauses) <- mconcat <$> mapM (go mnrs' sdc') children

        --         if fromMaybe True mtargetMatch || not (null childClauses)
        --             then do
        --                 let fullReturn =
        --                         [ Clause
        --                             [mkPathPat restP pats]
        --                             (NormalB $ helperE `AppE` restE)
        --                             [FunD helperName $ childClauses ++ [clause404 sdc]]]
        --                     passThru =
        --                         return childClauses

        --                 fmap ((,) (name : childNames)) $ case mtargetMatch of
        --                     Nothing ->
        --                         -- no match, or no matching. return full.
        --                         pure fullReturn
        --                     Just True ->
        --                         -- we are currently in a match.
        --                         passThru
        --                     Just False ->
        --                         pure fullReturn

        --             else do
        --                 -- Don't generate clauses for a nested thing we're
        --                 -- not targeting.
        --                 pure ([], [])

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
        case guard mdsNestedRouteFallthrough of
            Nothing -> do
                handler <- mds404
                runHandler <- mdsRunHandler
                let exp = runHandler `AppE` handler `AppE` envE `AppE` ConE 'Nothing `AppE` reqE
                return $ Clause [WildP] (NormalB exp) []
            Just () -> do
                return $ Clause [WildP] (NormalB (ConE 'Nothing)) []

-- | This function generates code to call 'yesodDispatchNested'.
nestedDispatchCall
    :: MkDispatchSettings w x z
    -- ^ Used for the 'mdsMethod'
    -> Exp
    -- ^ The @restExpr@ representing the remainder of the path pieces
    -> SDC
    -- ^ The accumulated 'SDC'.
    -> [Exp]
    -- ^ The dynamic bound variables from the route.
    -> Q Exp
nestedDispatchCall mds restExpr sdc dyns = do
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

-- | Wrap the call to 'yesodDispatchNested' in a manner appropriate to the
-- 'DispatchPhase' in the 'MkDispatchSettings'.
wrapNestedDispatchCall
    :: DispatchPhase
    -> MkDispatchSettings x y z
    -- ^ Used for 'mdsRunHandler'
    -> SDC
    -- ^ Accumulated route pieces
    -> Exp
    -- ^ The parent constructor expr for the route datatype
    -> Exp
    -- ^ The call to 'yesodDispatchNested'
    -> Q Exp
wrapNestedDispatchCall dispatchPhase mds sdc constrExpr hndlr =
    case dispatchPhase of
        TopLevelDispatch ->
            [e|
                $(mdsRunHandler mds)
                    (fst $(pure hndlr))
                    $(pure $ envExp sdc)
                    ($(pure constrExpr) <$> snd $(pure hndlr))
                    $(pure $ reqExp sdc)
            |]
        NestedDispatch ->
            [e|
                fmap $(pure constrExpr) $(pure hndlr)
            |]

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
    -> Type
    -- ^ The master site type
    -> Cxt
    -- ^ Context of the instance
    -> (Exp -> Q Exp)
    -- ^ Unwrap handler
    -> [ResourceTree Type]
    -- ^ The resource
    -> DecsQ
mkDispatchInstance routeOpts@(roFocusOnNestedRoute -> Nothing) master cxt unwrapper res = do
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
        phase = TopLevelDispatch
    (childNames, clause') <- mkDispatchClause phase mdsWithNestedDispatch res
    let thisDispatch = FunD 'yesodDispatch [clause']
    childInstances <-
        fmap mconcat $ forM childNames $ \name -> do
            mkNestedDispatchInstance routeOpts name master cxt unwrapper res
    return (instanceD cxt yDispatch [thisDispatch] : childInstances)
  where
    yDispatch = ConT ''YesodDispatch `AppT` master

mkDispatchInstance routeOpts@(roFocusOnNestedRoute -> Just target) master cxt unwrapper res = do
    mkNestedDispatchInstance routeOpts target master cxt unwrapper res

mkNestedDispatchInstance
    :: RouteOpts
    -> String
    -> Type
    -> Cxt
    -> (Exp -> Q Exp)
    -> [ResourceTree Type]
    -> Q [Dec]
mkNestedDispatchInstance routeOpts target master cxt unwrapper res = do
    let mstuff = findNestedRoute target res
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
            { mdsRunHandler =
                [| \handler _env mroute _req ->
                    Just (fmap toTypedContent handler, mroute)
                 |]
            , mdsGetPathInfo =
                [| snd |]
            , mdsMethod =
                [| fst |]
            , mdsHandleNestedRoute = NestedRouteSettings
                { nrsTargetName = Nothing
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

    (childNames, clause') <- mkDispatchClause NestedDispatch mdsWithNestedDispatch subres

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
            mkNestedDispatchInstance routeOpts name master cxt unwrapper res
    return
        ( instanceD cxt yDispatchNested
            [ thisDispatch
            ]
        : childInstances
        )

mkYesodSubDispatch :: [ResourceTree a] -> Q Exp
mkYesodSubDispatch res = do
    (childNames, clause') <-
        mkDispatchClause
            TopLevelDispatch
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
mkMDS unwrapper runHandler subDispatcher = MkDispatchSettings
    { mdsRunHandler = runHandler
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
