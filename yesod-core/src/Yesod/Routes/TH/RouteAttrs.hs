{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Yesod.Routes.TH.RouteAttrs
    ( mkRouteAttrsInstance
    , mkRouteAttrsInstanceFor
    , mkRouteAttrsNestedInstances
    ) where

import Yesod.Routes.TH.Types
import Yesod.Routes.TH.Internal (nestedInstanceExists, resolveRouteCon, instanceD, conPCompat)
import Yesod.Routes.Class
import Language.Haskell.TH.Syntax
import Data.Foldable (toList)
import qualified Data.Set  as Set
import qualified Data.Text as Text
import Control.Monad

mkRouteAttrsInstance :: Cxt -> Type -> [ResourceTree a] -> Q Dec
mkRouteAttrsInstance cxt typ ress = do
    clauses <- mapM (goTree Nothing id) ress
    return $ instanceD cxt (ConT ''RouteAttrs `AppT` typ)
        [ FunD 'routeAttrs $ concat clauses ++
                [Clause [ WildP ] (NormalB $ VarE 'mempty) []]
        ]

-- | Like 'mkRouteAttrsInstance', but uses a 'String' name of a nested
-- subroute to generate nested instances instead of generating the full
-- instance for all routes.
--
-- @since 1.7.0.0
mkRouteAttrsInstanceFor :: Cxt -> Type -> String -> [ResourceTree a] -> Q [Dec]
mkRouteAttrsInstanceFor cxt typ target ress = do
    clauses <- mapM (goTree (Just target) id) ress
    return [instanceD cxt (ConT ''RouteAttrsNested `AppT` typ)
        [ FunD 'routeAttrsNested $ concat clauses ++
                [Clause [ WildP ] (NormalB $ VarE 'mempty) []]
        ]
        ]

-- | Generate a 'RouteAttrsNested' instance for every nested parent route in
-- the tree (recursively), skipping any whose instance already exists (e.g. one
-- generated in another module when routes are split). This is the
-- 'RouteAttrs' counterpart to the @RenderRouteNested@ \/ @ParseRouteNested@ \/
-- @YesodDispatchNested@ instances that a plain 'mkYesod' already emits for its
-- child route fragments, so that @routeAttrsNested ChildR@ resolves for a
-- single-module site just like the other nested-delegation methods do.
--
-- @since 1.7.0.0
mkRouteAttrsNestedInstances :: Cxt -> TyArgs -> [ResourceTree a] -> Q [Dec]
mkRouteAttrsNestedInstances cxt tyargs ress =
    concat <$> mapM perParent (parentNames ress)
  where
    parentNames trees = concat
        [ name : parentNames children
        | ResourceParent name _ _ _ children <- trees
        ]
    perParent name = do
        exists <- nestedInstanceExists ''RouteAttrsNested =<< resolveRouteCon name
        if exists
            then pure []
            else mkRouteAttrsInstanceFor cxt (applyTyArgs (ConT (mkName name)) tyargs) name ress

goTree :: Maybe String -> (Pat -> Pat) -> ResourceTree a -> Q [Clause]
goTree mtarget front (ResourceLeaf res) =
    case mtarget of
        Nothing ->
            return $ toList $ goRes front res
        Just _ ->
            return []
goTree (Just target) front (ResourceParent name _check _attrs _pieces trees)
    | target /= name = concat <$> mapM (goTree (Just target) front) trees
goTree mtarget front (ResourceParent name _check _attrs pieces trees) = do
    doesNestedInstanceExist <- nestedInstanceExists ''RouteAttrsNested =<< resolveRouteCon name

    if doesNestedInstanceExist
        then do
            x <- newName "x"
            pure [Clause [front (conPCompat (mkName name) (ignored (VarP x)))] (NormalB $ VarE 'routeAttrsNested `AppE` VarE x) []]
        else do
            let mtarget' = do
                    target <- mtarget
                    guard (target /= name)
                    mtarget
                front'' =
                    case Just name == mtarget of
                        True ->
                            front
                        False ->
                            front'
            mainClauses <- concat <$> mapM (goTree mtarget' front'') trees
            pure mainClauses
  where
    ignored = (replicate toIgnore WildP ++) . return
    toIgnore = length $ filter isDynamic pieces
    isDynamic Dynamic{} = True
    isDynamic Static{} = False
    front' = front . conPCompat (mkName name)
                   . ignored

goRes :: (Pat -> Pat) -> Resource a -> Maybe Clause
goRes front Resource {..} = do
    guard (not (null resourceAttrs))
    return $ Clause
        [front $ RecP (mkName resourceName) []]
        (NormalB $ VarE 'Set.fromList `AppE` ListE (map toText resourceAttrs))
        []
  where
    toText s = VarE 'Text.pack `AppE` LitE (StringL s)
