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
            fragmentVar <- newName "fragment"
            constraint <- newName "constraint"
            clauses <- projectClauses rootLabel id [] trees
            lookups <- forM owners $ \(typ, label) -> do
                args <- newName "parentArgs"
                leaf <- newName "leaf"
                let selected = conPCompat 'SomeRouteLeaf
                        [conPCompat (witnessName label) [], VarP args, VarP leaf]
                    found = ConE 'Just `AppE` mkTupE [VarE args, VarE leaf]
                    fallback = [Clause [WildP] (NormalB $ ConE 'Nothing) [] | length owners > 1]
                pure $ instanceD context (ConT ''LookupRouteLeaves `AppT` typ)
                    [FunD 'lookupRouteLeaves (Clause [selected] (NormalB found) [] : fallback)]
            let witness fragmentType = ConT ''RouteFragmentWitness `AppT` root `AppT` fragmentType
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
            pure $ localViews ++ lookups ++
                [ dataInstanceD ''RouteFragmentWitness [root, VarT fragmentVar] constructors
                , instanceD dictContext
                    (ConT ''RouteFragmentDict `AppT` VarT constraint `AppT` root)
                    [FunD 'getRouteFragmentDict dictClauses]
                , instanceD context (ConT ''RouteLeafSelection `AppT` site)
                    [FunD 'selectRouteLeaf clauses]
                ]
  where
    childType name = applyTyArgs (ConT $ mkName name) tyargs

    localInstances typ trees = do
        let leaves = [res | ResourceLeaf res <- trees]
        own <- if null leaves then pure [] else do
            -- A child can be imported from an earlier focused data splice.
            -- Unresolved local datatypes have not been emitted yet.
            known <- case typ of
                AppT (ConT route) _ | route == ''Route -> isInstance ''HasRouteLeaves [typ]
                _ -> nestedInstanceExists ''HasRouteLeaves =<< resolveRouteCon (typeHeadName typ)
            if known then pure [] else do
                projections <- forM trees $ \tree -> case tree of
                    ResourceParent name _ _ _ _ ->
                        pure $ Clause [RecP (mkName name) []] (NormalB $ ConE 'Nothing) []
                    ResourceLeaf res -> do
                        vars <- fieldVars res
                        pure $ Clause [conPCompat (mkName $ resourceName res) (map VarP vars)]
                            (NormalB $ ConE 'Just `AppE` applyConstructor (leafName res) vars) []
                embeddings <- forM leaves $ \res -> do
                    vars <- fieldVars res
                    pure $ Clause [conPCompat (leafName res) (map VarP vars)]
                        (NormalB $ applyConstructor (mkName $ resourceName res) vars) []
                pure [instanceD context (ConT ''HasRouteLeaves `AppT` typ)
                    [ dataInstanceD ''RouteLeaves [typ]
                        [NormalC (leafName res) [(lazyField, t) | t <- leafFieldTypes res] | res <- leaves]
                    , FunD 'projectRouteLeaves projections
                    , FunD 'fromRouteLeaves embeddings
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
                    [ConE $ witnessName label, parentArgsExpr parents, applyConstructor (leafName res) vars]) []]

    fieldVars res = replicateM (length $ leafFieldTypes res) (newName "capture")
    leafName res = mkName $ "Leaf" ++ resourceName res
    witnessName label = mkName $ "Fragment" ++ label
    applyConstructor name = foldl' AppE (ConE name) . map VarE
    lazyField = Bang NoSourceUnpackedness NoSourceStrictness
    siteName = typeHeadName
    typeHeadName (ConT name) = nameBase name
    typeHeadName (AppT typ _) = typeHeadName typ
    typeHeadName typ = error $ "Unexpected route type head: " ++ show typ
