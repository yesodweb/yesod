{-# LANGUAGE TemplateHaskell #-}

-- | Policy-independent endpoint views and constraint dictionary selection.
module Yesod.Routes.TH.Leaf (mkRouteLeafData) where

import Control.Monad (forM, replicateM)
import Data.List (foldl')
import Language.Haskell.TH.Syntax
import Yesod.Routes.Class
import Yesod.Routes.Class.Leaf
import Yesod.Routes.TH.Internal
import Yesod.Routes.TH.Types

-- | Emit local views for each endpoint owner. A focused splice emits just its
-- subtree's local views; the full-site splice owns the projection and witness
-- table. Only existing structural instances are inspected, never policies.
mkRouteLeafData
    :: Cxt -> TyArgs -> Type -> Maybe String -> [ResourceTree Type] -> Q [Dec]
mkRouteLeafData context tyargs site focus resources = do
    (root, rootLabel, trees) <- case focus of
        Nothing -> pure (ConT ''Route `AppT` site, "Route" ++ siteName site, resources)
        Just target -> case findNestedRoute target resources of
            Nothing -> fail $ "Route leaf target '" ++ target ++ "' was not found."
            Just (_, children) -> pure (childType target, target, children)
    localViews <- localInstances root trees
    case focus of
        Just _ -> pure localViews
        Nothing -> do
            let owners = collectOwners root rootLabel trees
            if null owners
                then fail "setRouteLeafViews: the route tree must contain an endpoint."
                else pure ()
            routeVar <- newName "route"
            constraint <- newName "constraint"
            clauses <- projectClauses rootLabel id [] trees
            let witness routeType = ConT ''Subroute `AppT` root `AppT` routeType
                constructors =
                    [ GadtC [witnessName label] [] (witness typ)
                    | (typ, label) <- owners
                    ]
                dictContext = [VarT constraint `AppT` typ | (typ, _) <- owners]
                dictClauses =
                    [ Clause [conPCompat (witnessName label) []]
                        (NormalB $ ConE 'Dict) []
                    | (_, label) <- owners
                    ]
            pure $ localViews ++
                [ dataInstanceD ''Subroute [root, VarT routeVar] constructors
                , instanceD dictContext
                    (ConT ''SubrouteDict `AppT` VarT constraint `AppT` root)
                    [FunD 'getSubrouteDict dictClauses]
                , instanceD context (ConT ''RouteLeaves `AppT` site)
                    [FunD 'routeLeaf clauses]
                ]
  where
    childType name = applyTyArgs (ConT $ mkName name) tyargs

    localInstances typ trees = do
        let leaves = [res | ResourceLeaf res <- trees]
        own <- if null leaves then pure [] else do
            -- A child can be imported from an earlier focused data splice.
            -- Unresolved local datatypes have not been emitted yet.
            known <- case typ of
                AppT (ConT route) _ | route == ''Route -> isInstance ''HasAuthDispatch [typ]
                _ -> nestedInstanceExists ''HasAuthDispatch =<< resolveRouteCon (typeHeadName typ)
            if known then pure [] else do
                projections <- forM trees $ \tree -> case tree of
                    ResourceParent name _ _ _ _ ->
                        pure $ Clause [RecP (mkName name) []] (NormalB $ ConE 'Nothing) []
                    ResourceLeaf res -> do
                        vars <- fieldVars res
                        pure $ Clause [conPCompat (mkName $ resourceName res) (map VarP vars)]
                            (NormalB $ ConE 'Just `AppE` applyConstructor (authName res) vars) []
                embeddings <- forM leaves $ \res -> do
                    vars <- fieldVars res
                    pure $ Clause [conPCompat (authName res) (map VarP vars)]
                        (NormalB $ applyConstructor (mkName $ resourceName res) vars) []
                pure [instanceD context (ConT ''HasAuthDispatch `AppT` typ)
                    [ dataInstanceD ''AuthDispatch [typ]
                        [NormalC (authName res) [(lazyField, t) | t <- leafFieldTypes res] | res <- leaves]
                    , FunD 'projectAuthDispatch projections
                    , FunD 'fromAuthDispatch embeddings
                    ]]
        children <- forM [ (name, child) | ResourceParent name _ _ _ child <- trees ] $
            \(name, child) -> localInstances (childType name) child
        pure $ own ++ concat children

    collectOwners typ label trees =
        [(typ, label) | any isLeaf trees] ++ concat
            [collectOwners (childType name) name child | ResourceParent name _ _ _ child <- trees]
    isLeaf ResourceLeaf{} = True
    isLeaf _ = False

    projectClauses label front parents trees = fmap concat $ forM trees $ \tree -> case tree of
        ResourceParent name _ _ pieces children -> do
            vars <- replicateM (length [() | Dynamic _ <- pieces]) (newName "parent")
            let front' inner = front $ conPCompat (mkName name) (map VarP vars ++ [inner])
            projectClauses name front' (parents ++ vars) children
        ResourceLeaf res -> do
            vars <- fieldVars res
            pure [Clause [front $ conPCompat (mkName $ resourceName res) (map VarP vars)]
                (NormalB $ foldl' AppE (ConE 'SomeRouteLeaf)
                    [ConE $ witnessName label, parentArgsExpr parents, applyConstructor (authName res) vars]) []]

    fieldVars res = replicateM (length $ leafFieldTypes res) (newName "capture")
    authName res = mkName $ "Auth" ++ resourceName res
    witnessName label = mkName $ "Leaf" ++ label
    applyConstructor name = foldl' AppE (ConE name) . map VarE
    lazyField = Bang NoSourceUnpackedness NoSourceStrictness
    siteName = typeHeadName
    typeHeadName (ConT name) = nameBase name
    typeHeadName (AppT typ _) = typeHeadName typ
    typeHeadName typ = error $ "Unexpected route type head: " ++ show typ
