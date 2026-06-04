
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Yesod.Routes.TH.Internal where

import Prelude hiding (exp)
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Language.Haskell.TH.Syntax hiding (Arity)
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
-- like 'isInstance' require fully-applied types.
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

-- | The declared type-parameter arity of a 'RouteCon', or 0 when it isn't in
-- scope (an unresolved name has no knowable arity).
--
-- @since 1.7.0.0
routeConArity :: RouteCon -> Q Int
routeConArity = maybe (pure 0) typeArity . rcResolved

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

-- | Validate that a parameterized subsite's nested route datatype carries
-- exactly as many type parameters as the subsite has type arguments.
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
-- 'checkNestedSubArity' turns it into an actionable, attributable message
-- (suitable for 'fail').

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

-- | The arity proven equal on a successful 'checkNestedSubArity' — returned
-- rather than discarded as @()@, so the check parses rather than merely
-- validates.
--
-- @since 1.7.0.0
newtype Arity = Arity Int deriving (Eq, Show)

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
-- exactly as many type parameters as the subsite has type arguments, returning
-- the proven 'Arity' on success or a structured 'ArityMismatch' otherwise.
--
-- Pure so it can be unit-tested directly.
--
-- @since 1.7.0.0
checkNestedSubArity
    :: SubsiteName
    -> RouteName
    -> SubsiteArity
    -> RouteArity
    -> Either ArityMismatch Arity
checkNestedSubArity subName routeName (SubsiteArity subArgs) (RouteArity nestedArity)
    | subArgs == nestedArity = Right (Arity subArgs)
    | otherwise = Left $ ArityMismatch subName routeName (SubsiteArity subArgs) (RouteArity nestedArity)

-- | The actionable @fail@ message for an 'ArityMismatch'.
--
-- @since 1.7.0.0
arityMismatchMessage :: ArityMismatch -> String
arityMismatchMessage (ArityMismatch (SubsiteName subName) (RouteName routeName) (SubsiteArity subArgs) (RouteArity nestedArity)) =
    concat
        [ "mkYesodSubDispatchInstance: the nested route datatype `", routeName
        , "` has ", show nestedArity, " type parameter(s), but the subsite `"
        , subName, "` has ", show subArgs
        , ". The subroute datatypes must carry exactly the subsite's type "
        , "parameter(s) — generate the subsite's routes with "
        , "`mkYesodSubDataOpts (setParameterizedSubroute True defaultOpts) ...` "
        , "so a parameterized subsite gets parameterized subroutes."
        ]

-- | Turn a single route piece into a match pattern, returning the freshly
-- bound 'Name' for dynamic pieces (and 'Nothing' for static ones). Abstracted
-- over the fresh-'Name' supply @m@: the 'Q' code generators pass 'newName',
-- while the pure, deterministic inline-parse path passes a 'State' 'Int'
-- counter. This is the single source of truth for the
-- @ViewP fromPathPiece (Just x)@ dynamic encoding shared by the parse and
-- dispatch clause builders.
handlePieceM :: Applicative m => (String -> m Name) -> Piece a -> m (Pat, Maybe Name)
handlePieceM _ (Static str) = pure (LitP $ StringL str, Nothing)
handlePieceM fresh (Dynamic _) = mk <$> fresh "dyn"
  where
    mk x = (ViewP (VarE 'fromPathPiece) (conPCompat 'Just [VarP x]), Just x)

-- | The list form of 'handlePieceM' projecting the dynamic binders as 'Name's,
-- for callers that consume the bound variables directly (e.g. nested-dispatch
-- handler arguments).
handlePiecesNames :: Applicative m => (String -> m Name) -> [Piece a] -> m ([Pat], [Name])
handlePiecesNames fresh =
    fmap (\pps -> (map fst pps, mapMaybe snd pps)) . traverse (handlePieceM fresh)

-- | The list form of 'handlePieceM' projecting the dynamic binders as 'VarE'
-- expressions, for callers that rebuild a route by applying its constructor to
-- the captured pieces.
handlePiecesM :: Applicative m => (String -> m Name) -> [Piece a] -> m ([Pat], [Exp])
handlePiecesM fresh =
    fmap (\(pats, names) -> (pats, map VarE names)) . handlePiecesNames fresh

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
