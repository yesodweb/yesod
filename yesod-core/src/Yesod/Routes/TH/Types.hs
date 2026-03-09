{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveLift #-}

-- | Warning! This module is considered internal and may have breaking changes
module Yesod.Routes.TH.Types
    ( -- * Data types
      Resource (..)
    , ResourceTree (..)
    , Piece (..)
    , Dispatch (..)
    , CheckOverlap
    , FlatResource (..)
      -- ** Helper functions
    , resourceMulti
    , resourceTreePieces
    , resourceTreeName
    , flatten
    ) where

import Language.Haskell.TH.Syntax
import Data.Set (Set)

data ResourceTree typ
    = ResourceLeaf (Resource typ)
    | ResourceParent String CheckOverlap (Set String) [Piece typ] [ResourceTree typ]
    deriving (Lift, Show, Functor)

resourceTreePieces :: ResourceTree typ -> [Piece typ]
resourceTreePieces (ResourceLeaf r) = resourcePieces r
resourceTreePieces (ResourceParent _ _ _ x _) = x

resourceTreeName :: ResourceTree typ -> String
resourceTreeName (ResourceLeaf r) = resourceName r
resourceTreeName (ResourceParent x _ _ _ _) = x

data Resource typ = Resource
    { resourceName :: String
    , resourcePieces :: [Piece typ]
    , resourceDispatch :: Dispatch typ
    , resourceAttrs :: [String]
    , resourceCheck :: CheckOverlap
    }
    deriving (Lift, Show, Functor)

type CheckOverlap = Bool

data Piece typ = Static String | Dynamic typ
    deriving (Lift, Show)

instance Functor Piece where
    fmap _ (Static s)  = Static s
    fmap f (Dynamic t) = Dynamic (f t)

data Dispatch typ =
    Methods
        { methodsMulti :: Maybe typ -- ^ type of the multi piece at the end
        , methodsMethods :: [String] -- ^ supported request methods
        }
    | Subsite
        { subsiteType :: typ
        , subsiteFunc :: String
        }
    deriving (Lift, Show)

instance Functor Dispatch where
    fmap f (Methods a b) = Methods (fmap f a) b
    fmap f (Subsite a b) = Subsite (f a) b

resourceMulti :: Resource typ -> Maybe typ
resourceMulti Resource { resourceDispatch = Methods (Just t) _ } = Just t
resourceMulti _ = Nothing

data FlatResource a = FlatResource
    { frParentPieces :: [(String, [Piece a])]
    , frParentAttrs :: [(String, Set String)]
    -- ^ @since TODO
    , frName :: String
    , frPieces :: [Piece a]
    , frDispatch :: Dispatch a
    , frCheck :: Bool
    } deriving (Show)

flatten :: [ResourceTree a] -> [FlatResource a]
flatten =
    concatMap (go id id True)
  where
    go pp pa check' (ResourceLeaf (Resource a b c _ check)) =
        [FlatResource (pp []) (pa []) a b c (check' && check)]
    go pp pa check' (ResourceParent name check attrs pieces children) =
        concatMap (go (pp . ((name, pieces):)) (pa . ((name, attrs):)) (check && check')) children
