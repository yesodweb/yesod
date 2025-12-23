
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Yesod.Routes.TH.Internal where

import Prelude hiding (exp)
import Language.Haskell.TH.Syntax
import Web.PathPieces
import Data.Maybe (catMaybes, fromMaybe)
import Control.Monad
import Data.List (foldl')
import Control.Arrow (second)
import Yesod.Routes.TH.Types
import Data.Char (toLower)
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Trans as Trans


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

-- | Given a target 'String', find the 'ResourceParent' in the
-- @['ResourceTree' a]@ corresponding to that target and return it.
-- Also return the @['Piece' a]@ captures that precede it.
findNestedRoute :: String -> [ResourceTree a] -> Maybe ([Piece a], [ResourceTree a])
findNestedRoute _ [] = Nothing
findNestedRoute target (res : ress) =
    case res of
        ResourceLeaf _ ->
            findNestedRoute target ress
        ResourceParent name _overlap pieces children -> do
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
