
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Yesod.Routes.TH.Internal where

import Prelude hiding (exp)
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Syntax.Compat (Quote(..))
import Web.PathPieces (fromPathPiece, fromPathMultiPiece)
import Yesod.Routes.TH.Types

conPCompat :: Name -> [Pat] -> Pat
conPCompat n pats = ConP n
#if MIN_VERSION_template_haskell(2,18,0)
                         []
#endif
                         pats

instanceD :: Cxt -> Type -> [Dec] -> Dec
instanceD = InstanceD Nothing

mkTupE :: [Exp] -> Exp
mkTupE =
    TupE
#if MIN_VERSION_template_haskell(2,16,0)
        . fmap Just
#endif

-- | The expression form of a route's accumulated @ParentArgs@: @()@ when there
-- are no dynamic pieces, the lone expression when there is one, and a tuple of
-- them otherwise. This is the single spelling of the unit\/single\/tuple idiom
-- that recurred across the dispatch and render-route clause builders.
parentArgsExprFromExps :: [Exp] -> Exp
parentArgsExprFromExps []  = ConE '()
parentArgsExprFromExps [e] = e
parentArgsExprFromExps es  = mkTupE es

-- | 'parentArgsExprFromExps' for the common case where the arguments are plain
-- variable names.
parentArgsExpr :: [Name] -> Exp
parentArgsExpr = parentArgsExprFromExps . map VarE

-- | The /pattern/ form of a route's accumulated @ParentArgs@, for binding the
-- incoming parent arguments: @_@ when there are none (the @ParentArgs@ is @()@
-- and ignored), the lone variable when there is one, and a tuple otherwise.
parentArgsPat :: [Name] -> Pat
parentArgsPat []  = WildP
parentArgsPat [a] = VarP a
parentArgsPat as  = TupP (map VarP as)

-- | The /type/ form of a route's accumulated @ParentArgs@: the unit type when
-- there are no dynamic pieces, the lone type when there is one, and a tuple
-- type otherwise.
parentArgsType :: [Type] -> Type
parentArgsType []  = ConT ''()
parentArgsType [t] = t
parentArgsType ts  = foldl' AppT (TupleT (length ts)) ts

-- | The number of type parameters a type constructor declares (0 for a type
-- that isn't a data\/newtype\/type-synonym, or that we can't reify).
typeArity :: Name -> Q Int
typeArity typeName = do
    info <- reify typeName
    pure $ case info of
        TyConI (DataD _ _ vs _ _ _) -> length vs
        TyConI (NewtypeD _ _ vs _ _ _) -> length vs
        TyConI (TySynD _ vs _) -> length vs
        _ -> 0

-- | Look up a type by 'Name' and return it fully applied with fresh
-- type variables. This is needed because nested route datatypes may
-- have type parameters (e.g., @NestedR subsite@), and TH functions
-- like 'isInstance' require fully-applied (kind 'Type') heads.
--
-- The application is to the datatype's /own/ reified 'typeArity', so the head
-- is saturated exactly — neither under- nor over-applied — and is therefore
-- well-kinded by construction. This holds for higher-kinded parameters too:
-- a parameter declared @(f :: Type -> Type)@ is filled with a bare,
-- unannotated @'VarT' a@, and GHC infers @a :: Type -> Type@ from the
-- datatype's declared kind, so we don't need to reconstruct each parameter's
-- kind ourselves. (Saturating by some /other/ arity — e.g. a caller's
-- 'TyArgs' count — is what historically built ill-kinded heads and made
-- 'isInstance' throw; see 'nestedInstanceExists'.)
fullyApplyType :: Name -> Q Type
fullyApplyType typeName = do
    arity <- typeArity typeName
    vars <- mapM (\i -> VarT <$> newName ("a" ++ show i)) [1..arity]
    pure $ foldl' AppT (ConT typeName) vars

-- | A route datatype name paired with the result of resolving it in the
-- current splice environment. 'rcResolved' is 'Just' when the datatype is
-- already in scope (defined in an earlier splice or a separately compiled
-- module) and 'Nothing' when it is not yet — typically because it is being
-- generated in the same splice group.
--
-- The point is to resolve a route name /once/, at the boundary, and carry the
-- answer in a value rather than re-running 'lookupTypeName' at each use site
-- and independently re-deciding what an unresolved name means. The two
-- consumers that matter — \"does its nested instance already exist?\"
-- ('nestedInstanceExists') and \"what type head do I splice?\"
-- ('appliedRouteTypeCon') — then share a single resolution.
--
-- @since 1.7.0.0
data RouteCon = RouteCon
    { rcName     :: String       -- ^ the route datatype name as written
    , rcResolved :: Maybe Name   -- ^ the resolved 'Name', if in scope
    }

-- | Resolve a route datatype name with 'lookupTypeName'. This is the single
-- boundary at which a route name is looked up; downstream code branches on the
-- resulting 'RouteCon' instead of resolving again.
--
-- @since 1.7.0.0
resolveRouteCon :: String -> Q RouteCon
resolveRouteCon name = RouteCon name <$> lookupTypeName name

-- | Does an instance of the given (nested-discovery) class — e.g.
-- @''YesodDispatchNested@, @''RenderRouteNested@, @''ParseRouteNested@,
-- @''RouteAttrsNested@ — already exist for this route datatype? This is the
-- single delegation probe shared by all the generators: when it answers 'True'
-- the parent delegates to the existing instance, and when 'False' it inlines
-- (or generates) the child itself.
--
-- Two rules, applied uniformly so the probe can never abort a splice:
--
--   * An unresolved 'RouteCon' (the datatype is being generated in the same
--     splice) trivially has no instance yet, so the answer is 'False'.
--   * A resolved one is saturated by its /own/ reified arity via
--     'fullyApplyType' — never by the site's 'TyArgs'. Applying the site's
--     type args to a child that was generated at a different arity (e.g. a
--     kind-'Type' child of a parameterized site) builds an ill-kinded head,
--     and 'isInstance' /throws/ a kind error rather than returning 'False',
--     aborting the splice. Saturating by the datatype's own arity keeps the
--     probed head well-kinded in every case.
--
-- /Can the abstract parameters make 'isInstance' fail?/ No. Once the head is
-- well-kinded (above), 'isInstance' answers by head unification — \"could
-- /any/ visible instance match this head?\" — which is a total query: it
-- returns 'True' or 'False' but cannot crash on an abstract @'VarT' a@. The
-- one theoretical imprecision is /over-matching/: because the argument is an
-- unconstrained variable, an instance written only at a more specific type
-- (e.g. @instance C (R Int)@) would still unify with @C (R a)@ and report
-- 'True'. That is harmless here because our codegen emits the nested-discovery
-- instances uniformly, one per route datatype at a fully abstract parameter
-- (e.g. @instance YesodDispatchNested (R a)@), so \"could match\" and \"does
-- match\" coincide — there is no narrower instance for the probe to confuse a
-- variable with. (Higher-kinded parameters are covered by the same argument;
-- see 'fullyApplyType'.)
--
-- @since 1.7.0.0
nestedInstanceExists :: Name -> RouteCon -> Q Bool
nestedInstanceExists klass rc =
    case rcResolved rc of
        Nothing       -> pure False
        Just typeName -> do
            appliedT <- fullyApplyType typeName
            isInstance klass [appliedT]

-- | The fully-applied route 'Type' for a 'RouteCon', as used for an instance
-- head. This is the single place the \"saturate by site 'TyArgs' vs. by the
-- datatype's own reified arity\" choice is made:
--
--   * With explicit 'TyArgs' (a parameterized site) the supplied type
--     arguments are applied to the constructor — the resolved 'Name' if in
--     scope, otherwise the bare 'mkName' (so a datatype being generated in the
--     same splice still gets a head to reference).
--   * With 'NoTyArgs' a resolved datatype is reified and saturated with fresh
--     type variables (needed for e.g. @mkYesodSubDispatch@, which doesn't know
--     the parent's type args); an unresolved one falls back to the bare
--     constructor.
--
-- @since 1.7.0.0
appliedRouteTypeCon :: RouteCon -> TyArgs -> Q Type
appliedRouteTypeCon rc tyargs =
    case (rcResolved rc, tyargs) of
        (Just typeName, NoTyArgs)     -> fullyApplyType typeName
        (Just typeName, SomeTyArgs{}) -> pure $ applyTyArgs (ConT typeName) tyargs
        (Nothing,       _)            -> pure $ applyTyArgs (ConT (mkName (rcName rc))) tyargs

-- | 'appliedRouteTypeCon' starting from the route datatype's name as a
-- 'String': resolve it ('resolveRouteCon') and apply the type arguments. The
-- fallback for an unresolved name (the bare constructor) is why this can't be
-- a plain @'lookupTypeName' >>=@ that requires the name to be in scope.
appliedRouteTypeNamed :: String -> TyArgs -> Q Type
appliedRouteTypeNamed routeName tyargs = do
    rc <- resolveRouteCon routeName
    appliedRouteTypeCon rc tyargs

-- | A subsite's type-constructor name. Distinct from 'RouteName' so the two
-- can't be swapped at a 'checkNestedSubArity' call site without a type error.
--
-- @since 1.7.0.0
newtype SubsiteName = SubsiteName String deriving (Eq, Show)

-- | A nested route datatype's name. See 'SubsiteName'.
--
-- @since 1.7.0.0
newtype RouteName = RouteName String deriving (Eq, Show)

-- | How many type arguments a subsite has. Distinct from 'RouteArity' so the
-- two arities can't be swapped (the equality predicate is symmetric, but the
-- error message is not).
--
-- @since 1.7.0.0
newtype SubsiteArity = SubsiteArity Int deriving (Eq, Show)

-- | The declared type-parameter arity of a nested route datatype. See
-- 'SubsiteArity'.
--
-- @since 1.7.0.0
newtype RouteArity = RouteArity Int deriving (Eq, Show)

-- | A detected mismatch between a subsite's type-argument count and its nested
-- route datatype's declared arity. Carrying the four facts (rather than a
-- pre-rendered 'String') lets the caller decide how to surface it and lets
-- tests assert on structure instead of substring-matching a message.
--
-- @since 1.7.0.0
data ArityMismatch = ArityMismatch
    { amSubsiteName  :: SubsiteName
    , amRouteName    :: RouteName
    , amSubsiteArity :: SubsiteArity
    , amRouteArity   :: RouteArity
    } deriving (Eq, Show)

-- | Validate that a parameterized subsite's nested route datatype carries
-- exactly as many type parameters as the subsite has type arguments: 'Nothing'
-- when the arities match, or a structured 'ArityMismatch' otherwise.
--
-- 'mkYesodSubDispatchInstance' applies the subsite's type arguments to each
-- nested route datatype (e.g. @NestedR subsite@) and uses the result directly
-- as the head of a @YesodSubDispatchNested@ instance, which expects a fully
-- applied (kind 'Type') route. So the arities must match exactly:
--
--   * Too few parameters on the nested datatype (e.g. it was generated by
--     plain 'mkYesodSubData', kind 'Type') over-applies it.
--   * Too many leaves it partially applied (kind @Type -> ...@).
--
-- Either way the result is a kind error reported deep inside generated code.
-- Surfacing it here (the caller passes the 'ArityMismatch' to 'fail') turns
-- it into an actionable, attributable message.
--
-- Pure so it can be unit-tested directly.
--
-- @since 1.7.0.0
checkNestedSubArity
    :: SubsiteName
    -> RouteName
    -> SubsiteArity
    -> RouteArity
    -> Maybe ArityMismatch
checkNestedSubArity subName routeName (SubsiteArity subArgs) (RouteArity nestedArity)
    | subArgs == nestedArity = Nothing
    | otherwise = Just $ ArityMismatch subName routeName (SubsiteArity subArgs) (RouteArity nestedArity)

-- | Which generator is running the arity check. The arity facts are identical
-- either way; this only steers 'arityMismatchMessage' so the failure names the
-- right entry point and calls the parameterized thing by its correct term
-- (a top-level site reached through the nested-dispatch recursion is not a
-- \"subsite\").
--
-- @since 1.7.0.0
data ArityCallSite
    = SubsiteCall   -- ^ a subsite, generated by 'mkYesodSubDispatchInstance'
    | TopLevelCall  -- ^ a top-level site's nested-dispatch recursion
    deriving (Eq, Show)

-- | Run 'checkNestedSubArity' for a (possibly unresolved) nested route
-- datatype and 'fail' with 'arityMismatchMessage' on a mismatch. A no-op when
-- the datatype is not in scope — an unresolved name has no knowable arity, so
-- the error (if any) surfaces from the generated code instead. This is the
-- single arity-guard shared by 'mkYesodSubDispatchInstance' and the nested
-- dispatch-instance recursion.
--
-- @since 1.7.0.0
assertNestedSubArity :: ArityCallSite -> SubsiteName -> SubsiteArity -> RouteCon -> Q ()
assertNestedSubArity callSite subName subArity rc =
    case rcResolved rc of
        Nothing -> pure ()
        Just tyname -> do
            nestedArity <- typeArity tyname
            maybe (pure ()) (fail . arityMismatchMessage callSite) $
                checkNestedSubArity subName (RouteName (rcName rc)) subArity (RouteArity nestedArity)

-- | The actionable @fail@ message for an 'ArityMismatch', phrased for the
-- 'ArityCallSite' that detected it.
--
-- @since 1.7.0.0
arityMismatchMessage :: ArityCallSite -> ArityMismatch -> String
arityMismatchMessage callSite (ArityMismatch (SubsiteName subName) (RouteName routeName) (SubsiteArity subArgs) (RouteArity nestedArity)) =
    concat
        [ prefix, ": the nested route datatype `", routeName
        , "` has ", show nestedArity, " type parameter(s), but the ", term, " `"
        , subName, "` has ", show subArgs
        , ". The subroute datatypes must carry exactly the ", term, "'s type "
        , "parameter(s) — generate the ", term, "'s routes with "
        , "`mkYesodSubDataOpts (setParameterizedSubroute True defaultOpts) ...` "
        , "so a parameterized ", term, " gets parameterized subroutes."
        ]
  where
    (prefix, term) = case callSite of
        SubsiteCall  -> ("mkYesodSubDispatchInstance", "subsite")
        TopLevelCall -> ("mkYesod", "site")

-- | Turn a single route piece into a match pattern, returning the freshly
-- bound 'Name' for dynamic pieces (and 'Nothing' for static ones). The
-- fresh-'Name' supply is the 'Quote' @m@ it runs in: the 'Q' code generators
-- run at 'Q' (hygienic 'newName'), while the pure, deterministic inline-parse
-- path runs at a 'Quote' instance over a monotonic counter (see the test
-- harness). This is the single source of truth for the
-- @ViewP fromPathPiece (Just x)@ dynamic encoding shared by the parse and
-- dispatch clause builders.
handlePieceM :: Quote m => Piece a -> m (Pat, Maybe Name)
handlePieceM (Static str) = pure (LitP $ StringL str, Nothing)
handlePieceM (Dynamic _) = mk <$> newName "dyn"
  where
    mk x = (ViewP (VarE 'fromPathPiece) (conPCompat 'Just [VarP x]), Just x)

-- | The list form of 'handlePieceM' projecting the dynamic binders as 'Name's,
-- for callers that consume the bound variables directly (e.g. nested-dispatch
-- handler arguments).
handlePiecesNames :: Quote m => [Piece a] -> m ([Pat], [Name])
handlePiecesNames =
    fmap (\pps -> (map fst pps, mapMaybe snd pps)) . traverse handlePieceM

-- | The list form of 'handlePieceM' projecting the dynamic binders as 'VarE'
-- expressions, for callers that rebuild a route by applying its constructor to
-- the captured pieces.
handlePiecesM :: Quote m => [Piece a] -> m ([Pat], [Exp])
handlePiecesM =
    fmap (\(pats, names) -> (pats, map VarE names)) . handlePiecesNames

-- | Rebuild a route by applying its constructor (named by 'String', resolved
-- with 'mkName') to the captured dynamic-piece expressions — the companion to
-- 'handlePiecesM', which produces the @['Exp']@. This is the single spelling
-- of the @'foldl'' 'AppE' ('ConE' ('mkName' name))@ route-construction idiom
-- that recurred across the dispatch and parse clause builders.
applyConPieces :: String -> [Exp] -> Exp
applyConPieces name = foldl' AppE (ConE (mkName name))

-- | How a path match pattern ends, after its matched piece patterns. Only
-- these four shapes are ever valid as a path tail, so making it a closed type
-- (rather than an arbitrary 'Pat') stops a caller from silently matching the
-- wrong number of segments — e.g. binding a @rest@ where the path was meant to
-- end exactly.
data PathTail
    = EndExact      -- ^ @[]@ — the path ends exactly here
    | EndRest Name  -- ^ bind the remaining segments to the given variable
    | EndWild       -- ^ @_@ — ignore any remaining segments
    | EndMulti Name -- ^ a trailing multipiece: @'fromPathMultiPiece' -> 'Just' n@

-- | The 'Pat' a 'PathTail' denotes (the seed the matched piece patterns are
-- consed onto in 'mkPathPat').
pathTailPat :: PathTail -> Pat
pathTailPat EndExact     = conPCompat '[] []
pathTailPat (EndRest n)  = VarP n
pathTailPat EndWild      = WildP
pathTailPat (EndMulti n) = ViewP (VarE 'fromPathMultiPiece) (conPCompat 'Just [VarP n])

-- | Build a path match pattern: a list pattern of the given piece patterns
-- ending in the given 'PathTail'.
mkPathPat :: PathTail -> [Pat] -> Pat
mkPathPat tl = foldr (\x y -> conPCompat '(:) [x, y]) (pathTailPat tl)

-- | Generate a single-argument lambda expression with a fresh name.
-- Takes a base name hint, generates a unique 'Name', and passes it
-- to a callback that produces the lambda body. Returns the complete
-- 'LamE' expression.
--
-- For multi-argument lambdas, nest calls — @\\x y -> body@ is
-- equivalent to @\\x -> \\y -> body@.
mkLambda :: String -> (Name -> Q Exp) -> Q Exp
mkLambda hint mkBody = do
    n <- newName hint
    bdy <- mkBody n
    pure $ LamE [VarP n] bdy

-- | Given a target 'String', find the 'ResourceParent' in the
-- @['ResourceTree' a]@ corresponding to that target and return it.
-- Also return the @['Piece' a]@ captures that precede it.
findNestedRoute :: String -> [ResourceTree a] -> Maybe ([Piece a], [ResourceTree a])
findNestedRoute _ [] = Nothing
findNestedRoute target (res : ress) =
    case res of
        ResourceLeaf _ ->
            findNestedRoute target ress
        ResourceParent name _overlap _attrs pieces children -> do
            if name == target
                then Just (pieces, children)
                else
                    let mresult = findNestedRoute target children
                    in
                        case mresult of
                            Nothing -> do
                                findNestedRoute target ress
                            Just (typs, childRoute) -> do
                                Just (pieces <> typs, childRoute)
