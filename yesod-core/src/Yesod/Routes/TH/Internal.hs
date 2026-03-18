
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Yesod.Routes.TH.Internal where

import Prelude hiding (exp)
import Data.List (foldl')
import Language.Haskell.TH.Syntax
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

-- | Look up a type by 'Name' and return it fully applied with fresh
-- type variables. This is needed because nested route datatypes may
-- have type parameters (e.g., @NestedR subsite@), and TH functions
-- like 'isInstance' require fully-applied types.
fullyApplyType :: Name -> Q Type
fullyApplyType typeName = do
    info <- reify typeName
    let arity = case info of
            TyConI (DataD _ _ vs _ _ _) -> length vs
            TyConI (NewtypeD _ _ vs _ _ _) -> length vs
            TyConI (TySynD _ vs _) -> length vs
            _ -> 0
    vars <- mapM (\i -> VarT <$> newName ("a" ++ show i)) [1..arity]
    pure $ foldl' AppT (ConT typeName) vars

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
