{-# LANGUAGE TemplateHaskell #-}
module Yesod.Routes.TH.Types
    ( -- * Data types
      Resource (..)
    , ResourceTree (..)
    , Piece (..)
    , Dispatch (..)
    , CheckOverlap
      -- ** Helper functions
    , resourceMulti
    , resourceTreePieces
    , resourceTreeName
    ) where

import Language.Haskell.TH.Syntax
import Control.Arrow (second)

data ResourceTree typ = ResourceLeaf (Resource typ) | ResourceParent String [(CheckOverlap, Piece typ)] [ResourceTree typ]

resourceTreePieces :: ResourceTree typ -> [(CheckOverlap, Piece typ)]
resourceTreePieces (ResourceLeaf r) = resourcePieces r
resourceTreePieces (ResourceParent _ x _) = x

resourceTreeName :: ResourceTree typ -> String
resourceTreeName (ResourceLeaf r) = resourceName r
resourceTreeName (ResourceParent x _ _) = x

instance Functor ResourceTree where
    fmap f (ResourceLeaf r) = ResourceLeaf (fmap f r)
    fmap f (ResourceParent a b c) = ResourceParent a (map (second $ fmap f) b) $ map (fmap f) c

instance Lift t => Lift (ResourceTree t) where
    lift (ResourceLeaf r) = [|ResourceLeaf $(lift r)|]
    lift (ResourceParent a b c) = [|ResourceParent $(lift a) $(lift b) $(lift c)|]

data Resource typ = Resource
    { resourceName :: String
    , resourcePieces :: [(CheckOverlap, Piece typ)]
    , resourceDispatch :: Dispatch typ
    }
    deriving Show

type CheckOverlap = Bool

instance Functor Resource where
    fmap f (Resource a b c) = Resource a (map (second $ fmap f) b) (fmap f c)

instance Lift t => Lift (Resource t) where
    lift (Resource a b c) = [|Resource $(lift a) $(lift b) $(lift c)|]

data Piece typ = Static String | Dynamic typ
    deriving Show

instance Functor Piece where
    fmap _ (Static s) = (Static s)
    fmap f (Dynamic t) = Dynamic (f t)

instance Lift t => Lift (Piece t) where
    lift (Static s) = [|Static $(lift s)|]
    lift (Dynamic t) = [|Dynamic $(lift t)|]

data Dispatch typ =
    Methods
        { methodsMulti :: Maybe typ -- ^ type of the multi piece at the end
        , methodsMethods :: [String] -- ^ supported request methods
        }
    | Subsite
        { subsiteType :: typ
        , subsiteFunc :: String
        }
    deriving Show

instance Functor Dispatch where
    fmap f (Methods a b) = Methods (fmap f a) b
    fmap f (Subsite a b) = Subsite (f a) b

instance Lift t => Lift (Dispatch t) where
    lift (Methods Nothing b) = [|Methods Nothing $(lift b)|]
    lift (Methods (Just t) b) = [|Methods (Just $(lift t)) $(lift b)|]
    lift (Subsite t b) = [|Subsite $(lift t) $(lift b)|]

resourceMulti :: Resource typ -> Maybe typ
resourceMulti Resource { resourceDispatch = Methods (Just t) _ } = Just t
resourceMulti _ = Nothing
