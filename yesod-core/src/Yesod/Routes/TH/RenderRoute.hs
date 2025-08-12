{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}

module Yesod.Routes.TH.RenderRoute
    ( -- ** RenderRoute
      mkRenderRouteInstance
    , mkRenderRouteInstanceOpts
    , mkRouteCons
    , mkRouteConsOpts
    , mkRenderRouteClauses

    , RouteOpts
    , defaultOpts
    , setEqDerived
    , setShowDerived
    , setReadDerived
    , setFocusOnNestedRoute
    , roFocusOnNestedRoute
    ) where

import Yesod.Routes.TH.Types
import Language.Haskell.TH (conT)
import Language.Haskell.TH.Syntax
import Data.Bits (xor)
import Data.Maybe (maybeToList)
import Control.Monad (replicateM)
import Data.Text (pack)
import Web.PathPieces (PathPiece (..), PathMultiPiece (..))
import Yesod.Routes.Class

-- | General opts data type for generating yesod.
--
-- Contains options for what instances are derived for the route. Use the setting
-- functions on `defaultOpts` to set specific fields.
--
-- @since 1.6.25.0
data RouteOpts = MkRouteOpts
    { roDerivedEq   :: Bool
    , roDerivedShow :: Bool
    , roDerivedRead :: Bool
    , roFocusOnNestedRoute :: Maybe String
    -- ^ If this option is set, then we will only generate datatypes for
    -- the nested subroute that matches this string.
    --
    -- @since TODO
    }

-- | Default options for generating routes.
--
-- Defaults to all instances derived.
--
-- @since 1.6.25.0
defaultOpts :: RouteOpts
defaultOpts = MkRouteOpts True True True Nothing

-- | If you set this with @Just routeName@, then the code generation will
-- generate code for the @routeName@ to be imported in the main dispatch
-- class. This allows you to generate code in separate modules.
--
-- Example:
--
-- First, you would put your route definitions into their own file.
--
-- @
-- module App.Routes.Resources where
--
-- import Yesod.Core
--
-- appResources :: [ResourceTree String]
-- appResources = [parseRoutes|
--     /  HomeR GET
--
--     /nest NestR:
--         /     NestIndexR GET POST
--         /#Int NestShowR  GET POST
--
-- |]
-- @
--
-- We have defined a nested route called @NestR@ here. A nested route is
-- created with the @:@ after the route name.
--
-- Then, in a module for the route specifically, we can generate the route
-- datatype and instances for later hooking in to the main instance.
--
-- @
-- module App.Routes.NestR where
--
-- import App.Routes.Resources
-- import Yesod.Core
--
-- mkYesodOpts (setFocusOnNestedRoute (Just "NestR") defaultOptions) "App" appResources
-- @
--
-- If you only want to generate the datatypes, you can separate things
-- further using 'mkYesodDataOpts' and 'mkYesodDispatchOpts'.
--
-- Finally, import that type into your main yesod macro code.
--
-- @
-- module App where
--
-- import App.Routes.Resources (appResources)
-- import App.Routes.NestR (NestR(..))
--
-- mkYesod "App" appResources
-- @
--
-- The call to 'mkYesod' will delegate to the generated code for @NestR@
-- rather than regenerating it.
--
-- @since TODO
setFocusOnNestedRoute :: Maybe String -> RouteOpts -> RouteOpts
setFocusOnNestedRoute mstr rdo = rdo { roFocusOnNestedRoute = mstr }

-- |
--
-- @since 1.6.25.0
setEqDerived :: Bool -> RouteOpts -> RouteOpts
setEqDerived b rdo = rdo { roDerivedEq = b }

-- |
--
-- @since 1.6.25.0
setShowDerived :: Bool -> RouteOpts -> RouteOpts
setShowDerived b rdo = rdo { roDerivedShow = b }

-- |
--
-- @since 1.6.25.0
setReadDerived :: Bool -> RouteOpts -> RouteOpts
setReadDerived b rdo = rdo { roDerivedRead = b }

-- |
--
-- @since 1.6.25.0
instanceNamesFromOpts :: RouteOpts -> [Name]
instanceNamesFromOpts (MkRouteOpts eq shw rd _) = prependIf eq ''Eq $ prependIf shw ''Show $ prependIf rd ''Read []
    where prependIf b = if b then (:) else const id

-- | Generate the constructors of a route data type.
mkRouteCons :: [ResourceTree Type] -> Q ([Con], [Dec])
mkRouteCons = mkRouteConsOpts defaultOpts

-- | Generate the constructors of a route data type, with custom opts.
--
-- @since 1.6.25.0
mkRouteConsOpts :: RouteOpts -> [ResourceTree Type] -> Q ([Con], [Dec])
mkRouteConsOpts opts rttypes = do
    mconcat <$> mapM mkRouteCon rttypes
    -- So there's two sorts of things we can be generating here:
    -- 1. The `Route site` datatype
    -- 2. A child datatype for a nested route
    --
    -- Suppose someone has called `mkRouteConsOpts` with a "focus" set. We
    -- must recur
  where
    mkRouteCon (ResourceLeaf res) =
        case roFocusOnNestedRoute opts of
            Nothing -> do
                pure ([con], [])
            Just _ -> do
                -- If we are focusing on a specific nested subroute, then
                -- a Leaf cannot possibly be what we want.
                pure ([], [])
      where
        con = NormalC (mkName $ resourceName res)
            $ map (notStrict,)
            $ concat [singles, multi, sub]
        singles = concatMap toSingle $ resourcePieces res
        toSingle Static{} = []
        toSingle (Dynamic typ) = [typ]

        multi = maybeToList $ resourceMulti res

        sub =
            case resourceDispatch res of
                Subsite { subsiteType = typ } -> [ConT ''Route `AppT` typ]
                _ -> []

    mkRouteCon (ResourceParent name _check pieces children) = do
        let newOpts =
                case roFocusOnNestedRoute opts of
                    Nothing ->
                        opts
                    Just target ->
                        if target == name
                            then setFocusOnNestedRoute Nothing opts
                            else opts
        (cons, decs) <- mkRouteConsOpts newOpts children
        let conts = mapM conT $ instanceNamesFromOpts opts
        let childDataName = mkName name

        -- Generate the child datatype if it has not been generated
        -- already, but only if we are *not* focusing on some other
        -- datatype.
        mname' <- lookupTypeName name
        mdec <- case mname' of
            Just _ -> do
                -- datatype already exists, definitely don't generate it
                pure Nothing
            Nothing -> do
                case roFocusOnNestedRoute opts of
                    Just target | target /= name -> do
                        -- If we have a target, and this ain't it, don't
                        -- generate
                        pure Nothing
                    _ -> do
                        childData <- mkChildDataGen childDataName cons conts
                        childClauses <- mkRenderRouteClauses children
                        childInstances <- do
                            pure $ InstanceD Nothing [] (ConT ''RenderRouteNested `AppT` ConT childDataName)
                                [ FunD 'renderRouteNested childClauses
                                ]
                        pure $ Just (childData : [childInstances])

        return ([con], maybe id (<>) mdec decs)
      where
        con = NormalC (mkName name)
            $ map (notStrict,)
            $ singles ++ [ConT $ mkName name]

        singles = concatMap toSingle pieces
        toSingle Static{} = []
        toSingle (Dynamic typ) = [typ]

        mkChildDataGen childDataName cons conts =
#if MIN_VERSION_template_haskell(2,12,0)
            DataD [] childDataName [] Nothing cons <$> fmap (pure . DerivClause Nothing) conts
#else
            DataD [] childDataName [] Nothing cons <$> conts
#endif

-- | Clauses for the 'renderRoute' method.
mkRenderRouteClauses :: [ResourceTree Type] -> Q [Clause]
mkRenderRouteClauses =
    mapM go
  where
    isDynamic Dynamic{} = True
    isDynamic _ = False

    go (ResourceParent name _check pieces children) = do
        let cnt = length $ filter isDynamic pieces
        dyns <- replicateM cnt $ newName "dyn"
        child <- newName "child"
        let pat = conPCompat (mkName name) $ map VarP $ dyns ++ [child]

        pack' <- [|pack|]
        tsp <- [|toPathPiece|]
        let piecesSingle = mkPieces (AppE pack' . LitE . StringL) tsp pieces dyns

        typeExists <- lookupTypeName name
        hasNestInstance <- case typeExists of
            Just _ ->
                isInstance ''RenderRouteNested [ConT (mkName name)]
            Nothing ->
                pure False


        childRender <- newName "childRender"
        let rr = VarE childRender
        childClauses <-
            if hasNestInstance
            then
                pure [Clause [] (NormalB (VarE 'renderRouteNested)) []]
            else
                mkRenderRouteClauses children

        a <- newName "a"
        b <- newName "b"

        colon <- [|(:)|]
        let cons y ys = InfixE (Just y) colon (Just ys)
        let pieces' = foldr cons (VarE a) piecesSingle

        let body = LamE [TupP [VarP a, VarP b]] (TupE
#if MIN_VERSION_template_haskell(2,16,0)
                                                  $ map Just
#endif
                                                  [pieces', VarE b]
                                                ) `AppE` (rr `AppE` VarE child)

        return $ Clause [pat] (NormalB body) [FunD childRender childClauses]

    go (ResourceLeaf res) = do
        let cnt = length (filter isDynamic $ resourcePieces res) + maybe 0 (const 1) (resourceMulti res)
        dyns <- replicateM cnt $ newName "dyn"
        sub <-
            case resourceDispatch res of
                Subsite{} -> return <$> newName "sub"
                _ -> return []
        let pat = conPCompat (mkName $ resourceName res) $ map VarP $ dyns ++ sub

        pack' <- [|pack|]
        tsp <- [|toPathPiece|]
        let piecesSingle = mkPieces (AppE pack' . LitE . StringL) tsp (resourcePieces res) dyns

        piecesMulti <-
            case resourceMulti res of
                Nothing -> return $ ListE []
                Just{} -> do
                    tmp <- [|toPathMultiPiece|]
                    return $ tmp `AppE` VarE (last dyns)

        body <-
            case sub of
                [x] -> do
                    rr <- [|renderRoute|]
                    a <- newName "a"
                    b <- newName "b"

                    colon <- [|(:)|]
                    let cons y ys = InfixE (Just y) colon (Just ys)
                    let pieces = foldr cons (VarE a) piecesSingle

                    return $ LamE [TupP [VarP a, VarP b]] (TupE
#if MIN_VERSION_template_haskell(2,16,0)
                                                            $ map Just
#endif
                                                            [pieces, VarE b]
                                                          ) `AppE` (rr `AppE` VarE x)
                _ -> do
                    colon <- [|(:)|]
                    let cons a b = InfixE (Just a) colon (Just b)
                    return $ TupE
#if MIN_VERSION_template_haskell(2,16,0)
                      $ map Just
#endif
                      [foldr cons piecesMulti piecesSingle, ListE []]

        return $ Clause [pat] (NormalB body) []

    mkPieces _ _ [] _ = []
    mkPieces toText tsp (Static s:ps) dyns = toText s : mkPieces toText tsp ps dyns
    mkPieces toText tsp (Dynamic{}:ps) (d:dyns) = tsp `AppE` VarE d : mkPieces toText tsp ps dyns
    mkPieces _ _ (Dynamic _ : _) [] = error "mkPieces 120"

-- | Generate the 'RenderRoute' instance.
--
-- This includes both the 'Route' associated type and the
-- 'renderRoute' method.  This function uses both 'mkRouteCons' and
-- 'mkRenderRouteClasses'.
mkRenderRouteInstance :: Cxt -> Type -> [ResourceTree Type] -> Q [Dec]
mkRenderRouteInstance = mkRenderRouteInstanceOpts defaultOpts

-- | Generate the 'RenderRoute' instance.
--
-- This includes both the 'Route' associated type and the
-- 'renderRoute' method.  This function uses both 'mkRouteCons' and
-- 'mkRenderRouteClasses'.
--
-- @since 1.6.25.0
mkRenderRouteInstanceOpts :: RouteOpts -> Cxt -> Type -> [ResourceTree Type] -> Q [Dec]
mkRenderRouteInstanceOpts opts cxt typ ress = do
    cls <- mkRenderRouteClauses ress
    (cons, decs) <- mkRouteConsOpts opts ress
#if MIN_VERSION_template_haskell(2,15,0)
    did <- DataInstD [] Nothing (AppT (ConT ''Route) typ) Nothing cons <$> fmap (pure . DerivClause Nothing) (mapM conT (clazzes False))
    let sds = fmap (\t -> StandaloneDerivD Nothing cxt $ ConT t `AppT` ( ConT ''Route `AppT` typ)) (clazzes True)
#elif MIN_VERSION_template_haskell(2,12,0)
    did <- DataInstD [] ''Route [typ] Nothing cons <$> fmap (pure . DerivClause Nothing) (mapM conT (clazzes False))
    let sds = fmap (\t -> StandaloneDerivD Nothing cxt $ ConT t `AppT` ( ConT ''Route `AppT` typ)) (clazzes True)
#else
    did <- DataInstD [] ''Route [typ] Nothing cons <$> mapM conT (clazzes False)
    let sds = fmap (\t -> StandaloneDerivD cxt $ ConT t `AppT` ( ConT ''Route `AppT` typ)) (clazzes True)
#endif
    case roFocusOnNestedRoute opts of
        Nothing -> do
            return $ instanceD cxt (ConT ''RenderRoute `AppT` typ)
                [ did
                , FunD (mkName "renderRoute") cls
                ]
                : sds ++ decs
        Just _ -> do
            -- If we're generating routes for a subtarget, then we won't
            -- generate the top-level `RenderRoute`. Instead, we'll want to
            -- only generate the `decs` that are returned, plus the child
            -- class declaration, eventually.
            pure decs
  where
    clazzes standalone = if standalone `xor` null cxt then
          clazzes'
        else
          []
    clazzes' = instanceNamesFromOpts opts

notStrict :: Bang
notStrict = Bang NoSourceUnpackedness NoSourceStrictness

instanceD :: Cxt -> Type -> [Dec] -> Dec
instanceD = InstanceD Nothing

conPCompat :: Name -> [Pat] -> Pat
conPCompat n pats = ConP n
#if MIN_VERSION_template_haskell(2,18,0)
                         []
#endif
                         pats
