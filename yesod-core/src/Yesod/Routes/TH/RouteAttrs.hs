{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Yesod.Routes.TH.RouteAttrs
    ( mkRouteAttrsInstance
    ) where

import Yesod.Routes.TH.Types
import Yesod.Routes.Class
import Language.Haskell.TH.Syntax
import Data.Set (fromList)
import Data.Text (pack)
import Control.Monad

mkRouteAttrsInstance :: Cxt -> Type -> [ResourceTree a] -> Q Dec
mkRouteAttrsInstance cxt typ ress = do
    clauses <- mapM (goTree id) ress
    return $ instanceD cxt (ConT ''RouteAttrs `AppT` typ)
        [ FunD 'routeAttrs $ concat clauses
        ]

goTree :: (Pat -> Pat) -> ResourceTree a -> Q [Clause]
goTree front (ResourceLeaf res) = return $ goRes front res
goTree front (ResourceParent name _check pieces trees) = do
    mainClauses <- concat <$> mapM (goTree front') trees
    pure $ mainClauses <>
        [ Clause
            [ WildP ]
            (NormalB $ VarE 'mempty)
            []
        ]
  where
    ignored = (replicate toIgnore WildP ++) . return
    toIgnore = length $ filter isDynamic pieces
    isDynamic Dynamic{} = True
    isDynamic Static{} = False
    front' = front . ConP (mkName name)
#if MIN_VERSION_template_haskell(2,18,0)
                          []
#endif
                   . ignored

goRes :: (Pat -> Pat) -> Resource a -> [Clause]
goRes front Resource {..} = do
    guard (not (null resourceAttrs))
    return $ Clause
        [front $ RecP (mkName resourceName) []]
        (NormalB $ VarE 'fromList `AppE` ListE (map toText resourceAttrs))
        []
  where
    toText s = VarE 'pack `AppE` LitE (StringL s)

instanceD :: Cxt -> Type -> [Dec] -> Dec
instanceD = InstanceD Nothing
