{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Yesod.Core.Internal.TH
    ( mkYesod
    , mkYesodOpts

    , mkYesodWith

    , mkYesodData
    , mkYesodDataOpts

    , mkYesodSubData
    , mkYesodSubDataOpts

    , mkYesodWithParser
    , mkYesodWithParserOpts

    , mkYesodDispatch
    , mkYesodDispatchOpts

    , masterTypeSyns

    , mkYesodGeneral
    , mkYesodGeneralOpts

    , mkMDS
    , mkDispatchInstance

    , mkYesodSubDispatch
    , mkYesodSubDispatchInstance
    , mkYesodSubDispatchInstanceOpts

    , subTopDispatch

    , RouteOpts
    , defaultOpts
    , setEqDerived
    , setShowDerived
    , setReadDerived
    , setCreateResources
    , setFocusOnNestedRoute
    , unsetFocusOnNestedRoute
    , setParameterizedSubroute
    , setNestedRouteFallthrough
    )
 where

import Prelude hiding (exp)
import Yesod.Core.Handler
import Language.Haskell.TH hiding (cxt, instanceD)
import Language.Haskell.TH.Syntax
import Data.ByteString.Lazy.Char8 ()
import Data.List (foldl')
import Data.Maybe
import Control.Monad
import Yesod.Routes.TH
import Yesod.Routes.TH.Dispatch (parseYesodName, mkYesodSubDispatchWithDelegate)
import Yesod.Routes.TH.Internal (instanceD, typeArity, assertNestedSubArity, ArityCallSite(..), SubsiteName(..), SubsiteArity(..), resolveRouteCon, nestedInstanceExists)
import Yesod.Routes.Parse
import Yesod.Core.Types
import Yesod.Core.Class.Dispatch (YesodSubDispatch(..), YesodSubDispatchNested(..))

-- | Generates URL datatype and site function for the given 'Resource's. This
-- is used for creating sites, /not/ subsites. See 'mkYesodSubData' and 'mkYesodSubDispatch' for the latter.
-- Use 'parseRoutes' to create the 'Resource's.
--
-- Contexts and type variables in the name of the datatype are parsed.
-- For example, a datatype @App a@ with typeclass constraint @MyClass a@ can be written as @\"(MyClass a) => App a\"@.
mkYesod :: String -- ^ name of the argument datatype
        -> [ResourceTree String]
        -> Q [Dec]
mkYesod = mkYesodOpts defaultOpts

-- | `mkYesod` but with custom options.
--
-- @since 1.6.25.0
mkYesodOpts :: RouteOpts
            -> String
            -> [ResourceTree String]
            -> Q [Dec]
mkYesodOpts opts name = fmap (uncurry (++)) . mkYesodWithParserOpts opts name False return


{-# DEPRECATED mkYesodWith "Contexts and type variables are now parsed from the name in `mkYesod`. <https://github.com/yesodweb/yesod/pull/1366>" #-}
-- | Similar to 'mkYesod', except contexts and type variables are not parsed.
-- Instead, they are explicitly provided.
-- You can write @(MyClass a) => App a@ with @mkYesodWith [[\"MyClass\",\"a\"]] \"App\" [\"a\"] ...@.
mkYesodWith :: [[String]] -- ^ list of contexts
            -> String -- ^ name of the argument datatype
            -> [String] -- ^ list of type variables
            -> [ResourceTree String]
            -> Q [Dec]
mkYesodWith cxts name args = fmap (uncurry (++)) . mkYesodGeneral cxts name args False return


-- | Sometimes, you will want to declare your routes in one file and define
-- your handlers elsewhere. For example, this is the only way to break up a
-- monolithic file into smaller parts. Use this function, paired with
-- 'mkYesodDispatch', to do just that.
mkYesodData :: String -> [ResourceTree String] -> Q [Dec]
mkYesodData = mkYesodDataOpts defaultOpts

-- | `mkYesodData` but with custom options.
--
-- @since 1.6.25.0
mkYesodDataOpts :: RouteOpts -> String -> [ResourceTree String] -> Q [Dec]
mkYesodDataOpts opts name resS = fst <$> mkYesodWithParserOpts opts name False return resS


mkYesodSubData :: String -> [ResourceTree String] -> Q [Dec]
mkYesodSubData = mkYesodSubDataOpts defaultOpts

-- |
--
-- @since 1.6.25.0
mkYesodSubDataOpts :: RouteOpts -> String -> [ResourceTree String] -> Q [Dec]
mkYesodSubDataOpts opts name resS = fst <$> mkYesodWithParserOpts opts name True return resS


-- | Parses contexts and type arguments out of name before generating TH.
mkYesodWithParser :: String                    -- ^ foundation type
                  -> Bool                      -- ^ is this a subsite
                  -> (Exp -> Q Exp)            -- ^ unwrap handler
                  -> [ResourceTree String]
                  -> Q([Dec],[Dec])
mkYesodWithParser = mkYesodWithParserOpts defaultOpts

-- | Parses contexts and type arguments out of name before generating TH.
--
-- @since 1.6.25.0
mkYesodWithParserOpts :: RouteOpts                 -- ^ Additional route options
                      -> String                    -- ^ foundation type
                      -> Bool                      -- ^ is this a subsite
                      -> (Exp -> Q Exp)            -- ^ unwrap handler
                      -> [ResourceTree String]
                      -> Q([Dec],[Dec])
mkYesodWithParserOpts opts name isSub f resS = do
    (name', rest, cxt) <- case parseYesodName name of
            Left err -> fail err
            Right a -> pure a

    mkYesodGeneralOpts opts cxt name' rest isSub f resS


-- | See 'mkYesodData'.
mkYesodDispatch :: String -> [ResourceTree String] -> Q [Dec]
mkYesodDispatch = mkYesodDispatchOpts defaultOpts

-- | See 'mkYesodDataOpts'
--
-- @since 1.6.25.0
mkYesodDispatchOpts :: RouteOpts -> String -> [ResourceTree String] -> Q [Dec]
mkYesodDispatchOpts opts name = fmap snd . mkYesodWithParserOpts opts name False return


-- | Build an instance context ('Cxt') from the parsed @=>@ class-application
-- groups produced by 'parseYesodName'. Each group is a class name applied to
-- type arguments, e.g. @["MyClass","a"]@ becomes @MyClass a@. Shared by
-- 'mkYesodGeneralOpts' and 'mkYesodSubDispatchInstanceOpts'.
buildAppCxt :: [[String]] -> Q Cxt
buildAppCxt = traverse $ \ctxs ->
    case ctxs of
        c:rest ->
            pure $ foldl' (\acc v -> acc `AppT` fst (nameToType v)) (ConT $ mkName c) rest
        [] -> fail $ "mkYesod: empty type-class context in route definition: " ++ show ctxs

-- | Get the Handler and Widget type synonyms for the given site.
masterTypeSyns :: [Name] -> Type -> [Dec] -- FIXME remove from here, put into the scaffolding itself?
masterTypeSyns vs site =
    [ TySynD (mkName "Handler") (fmap plainTV vs)
      $ ConT ''HandlerFor `AppT` site
    , TySynD (mkName "Widget")  (fmap plainTV vs)
      $ ConT ''WidgetFor `AppT` site `AppT` ConT ''()
    ]


mkYesodGeneral :: [[String]]                -- ^ Appliction context. Used in RenderRoute, RouteAttrs, and ParseRoute instances.
               -> String                    -- ^ foundation type
               -> [String]                  -- ^ arguments for the type
               -> Bool                      -- ^ is this a subsite
               -> (Exp -> Q Exp)            -- ^ unwrap handler
               -> [ResourceTree String]
               -> Q([Dec],[Dec])
mkYesodGeneral = mkYesodGeneralOpts defaultOpts

-- | Convert the parsed-from-source 'String' route types into real 'Type's at a
-- splice site. A malformed type or an unclosed @#{…}@ bracket surfaces as an
-- attributed compile error (via 'fail') rather than a raw 'error' thrown lazily
-- when the resulting tree is forced. The pure 'parseType'\/'dropBracket' are
-- kept for callers (e.g. tests) that supply known-good input.
parseResourceTypes :: [ResourceTree String] -> Q [ResourceTree Type]
parseResourceTypes = traverse (traverse (\s -> dropBracketM s >>= parseTypeM))

-- | The resolved foundation type shared by 'mkYesodGeneralOpts' and
-- 'mkYesodSubDispatchInstanceOpts': the @boundNames@ from the explicitly-written
-- type args plus enough fresh @t@ variables to fill the reified arity, the fully
-- applied @site@ type, and the parsed resources. The callers differ only in how
-- they roll these into 'TyArgs' (and the synonym-head vars), so that part stays
-- with each caller.
data ResolvedFoundation = ResolvedFoundation
    { rfBoundNames :: [(Type, Name)]  -- ^ explicitly-written type args
    , rfFillVars   :: [Name]          -- ^ fresh vars filling the reified arity
    , rfSite       :: Type            -- ^ the fully applied site type
    , rfResources  :: [ResourceTree Type]
    }

resolveFoundation :: String -> [String] -> [ResourceTree String] -> Q ResolvedFoundation
resolveFoundation namestr mtys resS = do
    mname <- lookupTypeName namestr
    arity <- maybe (pure 0) typeArity mname
    let name = mkName namestr
    -- Generate as many variable names as the arity indicates
    vns <- replicateM (arity - length mtys) $ newName "t"
    let boundNames = fmap nameToType mtys
        argtypes = fmap fst boundNames ++ fmap VarT vns
        site = foldl' AppT (ConT name) argtypes
    res <- parseResourceTypes resS
    pure ResolvedFoundation
        { rfBoundNames = boundNames
        , rfFillVars = vns
        , rfSite = site
        , rfResources = res
        }

-- |
--
-- @since 1.6.25.0
mkYesodGeneralOpts :: RouteOpts                 -- ^ Options to adjust route creation
                   -> [[String]]                -- ^ Application context. Used in RenderRoute, RouteAttrs, and ParseRoute instances.
                   -> String                    -- ^ foundation type
                   -> [String]                  -- ^ arguments for the type
                   -> Bool                      -- ^ is this a subsite
                   -> (Exp -> Q Exp)            -- ^ unwrap handler
                   -> [ResourceTree String]
                   -> Q([Dec],[Dec])
mkYesodGeneralOpts opts appCxt' namestr mtys isSub f resS = do
    appCxt <- buildAppCxt appCxt'
    foundation <- resolveFoundation namestr mtys resS
    -- The explicitly-written args plus the fresh vars filling the reified
    -- arity. Both are part of the site's full application ('rfSite'), so both
    -- must travel in the 'TyArgs' handed to 'discoveryMode' and the generators
    -- — otherwise a parameterized foundation invoked without explicit type args
    -- would be misclassified as monomorphic and emit ill-scoped nested
    -- instances.
    let fillNames = fmap (\v -> (VarT v, v)) (rfFillVars foundation)
        tyArgs = toTyArgs (rfBoundNames foundation ++ fillNames)
    -- typevars that should appear in synonym head
    let argvars = (fmap mkName . filter isTvar) mtys ++ rfFillVars foundation
    renderRouteDec <-
        mkRenderRouteInstanceOpts opts appCxt tyArgs (rfSite foundation) (rfResources foundation)
    routeAttrsDec  <-
        case roFocusOnNestedRoute opts of
            Nothing -> do
                -- The flat 'RouteAttrs (Route site)' instance, plus — in
                -- nested-discovery mode — a 'RouteAttrsNested' instance for each
                -- child fragment, mirroring the RenderRouteNested /
                -- ParseRouteNested / YesodDispatchNested instances generated for
                -- the same children. Without this, 'routeAttrsNested ChildR'
                -- would fail to resolve for a single-module 'mkYesod' site even
                -- though the other nested-delegation methods resolve.
                flatAttrs <- mkRouteAttrsInstance appCxt (rfSite foundation) (rfResources foundation)
                nestedAttrs <-
                    case discoveryMode opts (hasTyArgs tyArgs) of
                        NestedDiscovery ->
                            mkRouteAttrsNestedInstances appCxt tyArgs (rfResources foundation)
                        InlineCompat    -> pure []
                pure (flatAttrs : nestedAttrs)
            Just target ->
                -- Apply the site's type arguments to the focused child's
                -- constructor, matching the focused ParseRoute / RenderRoute
                -- paths; a bare 'ConT' here is a kind error for a
                -- parameterized site.
                mkRouteAttrsInstanceFor
                    appCxt
                    (applyTyArgs (ConT (mkName target)) tyArgs)
                    target
                    (rfResources foundation)

    dispatchDec <-
        mkDispatchInstance opts (rfSite foundation) appCxt tyArgs f (rfResources foundation)
    parseRoute <-
        mkParseRouteInstanceOpts opts tyArgs appCxt (rfSite foundation) (rfResources foundation)
    let rname = mkName $ "resources" ++ namestr
    resourcesDec <-
        if shouldCreateResources opts
            then do
                eres <- lift resS
                pure
                    [ SigD rname $ ListT `AppT` (ConT ''ResourceTree `AppT` ConT ''String)
                    , FunD rname [Clause [] (NormalB eres) []]
                    ]
            else do
                pure []
    let dataDec = concat
            [ parseRoute
            , renderRouteDec
            , routeAttrsDec
            , if isJust (roFocusOnNestedRoute opts) then [] else resourcesDec
            , if isSub || isJust (roFocusOnNestedRoute opts)
                then []
                else masterTypeSyns argvars (rfSite foundation)
            ]
    return (dataDec, dispatchDec)

-- | Generate both 'YesodSubDispatch' and 'YesodSubDispatchNested' instances
-- for a parameterized subsite. This is the subsite equivalent of using
-- @mkYesod@ for top-level sites.
--
-- Usage:
--
-- @
-- mkYesodSubDispatchInstance "(MyClass a) => MySub a" resourcesMySub
-- @
--
-- This generates:
--
-- 1. A 'YesodSubDispatch' instance using 'mkYesodSubDispatch'
-- 2. 'YesodSubDispatchNested' instances for any nested route fragments
--
-- The generated instances quantify over the @master@ site and constrain it
-- only through the subsite's own class. For a parameterized subsite this
-- normally means the subsite's class carries a @subsite -> master@ functional
-- dependency (so @master@ is determined) and the using module enables
-- @UndecidableInstances@ (the instance contexts mention type-family
-- applications and non-variable arguments), along with @FlexibleContexts@,
-- @FlexibleInstances@, @MultiParamTypeClasses@ and @TypeFamilies@. The nested
-- route datatype must also declare exactly as many type parameters as the
-- subsite has type arguments (see the arity note below).
--
-- @since 1.7.0.0
mkYesodSubDispatchInstance
    :: String                -- ^ Foundation type with optional context, e.g. @"(MyClass a) => MySub a"@
    -> [ResourceTree String] -- ^ Resources (e.g. @resourcesMySub@)
    -> Q [Dec]
mkYesodSubDispatchInstance = mkYesodSubDispatchInstanceOpts defaultOpts

-- | Like 'mkYesodSubDispatchInstance', but takes a 'RouteOpts' so a subsite can
-- control nested-route generation — most usefully 'setNestedRouteFallthrough'
-- (which 'mkYesodSubDispatchInstance' leaves at its default of 'False'). The
-- flag is threaded into both the @yesodSubDispatch@ body (so the subsite's own
-- top-level parent clauses fall through to later siblings on an inner miss) and
-- the generated @YesodSubDispatchNested@ fragment instances.
--
-- @since 1.7.0.0
mkYesodSubDispatchInstanceOpts
    :: RouteOpts
    -> String                -- ^ Foundation type with optional context, e.g. @"(MyClass a) => MySub a"@
    -> [ResourceTree String] -- ^ Resources (e.g. @resourcesMySub@)
    -> Q [Dec]
mkYesodSubDispatchInstanceOpts opts nameStr resS = do
    -- Parse the name string to extract context, type name, and type args
    -- (the same parser the top-level mkYesod entry points use).
    (namestr, mtys, appCxt') <- case parseYesodName nameStr of
        Left err -> fail err
        Right a -> pure a

    appCxt <- buildAppCxt appCxt'

    foundation <- resolveFoundation namestr mtys resS
    -- The explicitly-written args plus the fresh vars filling the reified
    -- arity. Both are part of the subsite's full application ('rfSite'), so both
    -- must travel in the 'TyArgs' handed to 'discoveryMode' and the generators
    -- — otherwise a parameterized subsite invoked without explicit type args
    -- (e.g. @mkYesodSubDispatchInstance "MySub"@ for @MySub a@) would be
    -- misclassified as monomorphic and emit ill-scoped nested instances. This
    -- mirrors the 'fillNames' fix in 'mkYesodGeneralOpts'.
    let fillNames = fmap (\v -> (VarT v, v)) (rfFillVars foundation)
        tyArgs = toTyArgs (rfBoundNames foundation ++ fillNames)

    -- Generate the YesodSubDispatch instance
    masterN <- newName "master"
    let masterT = VarT masterN
        -- Thread the opts (in particular 'roNestedRouteFallthrough') into the
        -- generated @yesodSubDispatch@ body so a subsite's own top-level parent
        -- clauses honor 'setNestedRouteFallthrough', matching the top-level
        -- 'mkTopLevelDispatchInstance' path. Pass 'True' for delegateInline:
        -- this entry point emits a 'YesodSubDispatchNested' instance per parent
        -- in this same splice (below), so the flat @yesodSubDispatch@ body
        -- delegates each parent to that instance instead of inlining the
        -- subtree dispatch (which the nested instance would re-emit), avoiding
        -- the doubled codegen. This mirrors 'mkTopLevelDispatchInstance' on the
        -- top-level path. Standalone 'mkYesodSubDispatch'/'mkYesodSubDispatchWith'
        -- keep delegateInline 'False' (no same-splice instances to delegate to).
        subDispatchExp = mkYesodSubDispatchWithDelegate True opts (rfResources foundation)
    subDispatchBody <- subDispatchExp
    let yesodSubDispatchInst = instanceD
            appCxt
            (ConT ''YesodSubDispatch `AppT` rfSite foundation `AppT` masterT)
            [ FunD 'yesodSubDispatch
                [ Clause [] (NormalB subDispatchBody) [] ]
            ]

    -- Find nested routes and generate YesodSubDispatchNested instances
    let findNested :: [ResourceTree a] -> [String]
        findNested [] = []
        findNested (ResourceParent n _ _ _ _ : rest) = n : findNested rest
        findNested (_ : rest) = findNested rest
        nestedNames = findNested (rfResources foundation)

    nestedInstances <- fmap mconcat $ forM nestedNames $ \nestedName -> do
        -- Resolve the nested datatype once and reuse it for both the
        -- "instance already exists?" probe and the arity check below.
        -- 'nestedInstanceExists' saturates by the datatype's own reified arity,
        -- so it can't throw a kind error on an arity-mismatched in-scope
        -- datatype — which is exactly the misuse the 'checkNestedSubArity'
        -- below diagnoses. That keeps the friendly arity check reachable
        -- instead of being preempted by a cryptic kind error from the probe.
        rc <- resolveRouteCon nestedName
        instanceExists <- nestedInstanceExists ''YesodSubDispatchNested rc
        if instanceExists
            then pure []
            else do
                -- This API targets parameterized subsites whose nested subroute
                -- datatypes carry the parent's type arguments (the subsite's
                -- 'tyArgs' are applied to the nested datatype directly below).
                -- Guard the misuse where the
                -- subsite is parameterized but the nested datatype is not:
                -- otherwise applying the subsite's type args to a kind-'Type'
                -- datatype produces a cryptic kind error from generated code.
                -- Only check when the datatype actually resolved — an
                -- unresolved name has no knowable arity (defaulting it to 0
                -- would wrongly report "0 type parameter(s)"), and downstream
                -- codegen reports the not-in-scope case on its own.
                assertNestedSubArity
                    SubsiteCall
                    (SubsiteName namestr)
                    (SubsiteArity (tyArgsArity tyArgs))
                    rc
                -- 'mkNestedSubDispatchInstance' applies 'tyArgs' to the nested
                -- datatype directly and never consults 'roParameterizedSubroute'
                -- (it only reads 'roNestedRouteFallthrough'), so passing 'opts'
                -- through unchanged is correct — forcing 'setParameterizedSubroute'
                -- here was a no-op.
                mkNestedSubDispatchInstance
                    opts
                    nestedName appCxt tyArgs return (rfResources foundation)

    return $ yesodSubDispatchInst : nestedInstances
