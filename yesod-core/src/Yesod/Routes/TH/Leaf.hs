{-# LANGUAGE TemplateHaskell #-}

-- | Policy-independent views of each fragment's direct endpoints.
module Yesod.Routes.TH.Leaf (mkRouteLeafData) where

import Control.Monad (forM, replicateM)
import Data.List (foldl')
import Language.Haskell.TH.Syntax
import Yesod.Routes.Class
import Yesod.Routes.Class.Leaf
import Yesod.Routes.TH.Internal
import Yesod.Routes.TH.Types

-- | Emit local views for each endpoint owner. A focused splice emits just its
-- subtree's local views. Only structural instances are inspected, never policies.
mkRouteLeafData
    :: Cxt -> TyArgs -> Type -> Maybe String -> [ResourceTree Type] -> Q [Dec]
mkRouteLeafData context tyargs site focus resources = do
    (root, trees) <- case focus of
        Nothing -> pure (ConT ''Route `AppT` site, resources)
        Just target -> case findNestedRoute target resources of
            Nothing -> fail $ "Route leaf target '" ++ target ++ "' was not found."
            Just (_, children) -> pure (childType target, children)
    localInstances root trees
  where
    childType name = applyTyArgs (ConT $ mkName name) tyargs

    localInstances typ trees = do
        let leaves = [res | ResourceLeaf res <- trees]
        own <- if null leaves then pure [] else do
            -- A child can be imported from an earlier focused data splice.
            -- Unresolved local datatypes have not been emitted yet.
            known <- case typ of
                AppT (ConT route) _ | route == ''Route -> isInstance ''HasRouteLeaves [typ]
                _ -> do
                    name <- typeHeadName typ
                    nestedInstanceExists ''HasRouteLeaves =<< resolveRouteCon name
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

    fieldVars res = replicateM (length $ leafFieldTypes res) (newName "capture")
    leafName res = mkName $ "Leaf" ++ resourceName res
    applyConstructor name = foldl' AppE (ConE name) . map VarE
    lazyField = Bang NoSourceUnpackedness NoSourceStrictness
    typeHeadName (ConT name) = pure $ nameBase name
    typeHeadName (AppT typ _) = typeHeadName typ
    typeHeadName typ = fail $ "Unexpected route type head: " ++ show typ
