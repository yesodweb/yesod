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
    , ParentDetails (..)
      -- * Route datatype type arguments
    , TyArgs (..)
    , toTyArgs
    , tyArgsList
    , tyArgsTypes
    , tyArgsBinders
    , tyArgsArity
    , hasTyArgs
    , applyTyArgs
      -- ** Helper functions
    , resourceMulti
    , resourceTreePieces
    , resourceTreeName
    , flatten
    ) where

import Language.Haskell.TH.Syntax
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Set (Set)
-- Provides Lift instance for Set in older versions of GHC
import Instances.TH.Lift ()

-- | The type arguments of a route datatype, paired with the source 'Name' each
-- was derived from. A monomorphic site like @App@ has 'NoTyArgs'; a
-- parameterized one like @MySub subsite@ has 'SomeTyArgs'.
--
-- Carrying the (non-)emptiness in the type — rather than as a bare list with a
-- separate @null@ check — keeps \"are there type args?\" and \"what are they?\"
-- a single fact, so the populated branch hands you the 'NonEmpty' directly.
--
-- @since 1.6.30.0
data TyArgs
    = NoTyArgs
    | SomeTyArgs (NonEmpty (Type, Name))
    deriving (Eq, Show)

-- | Build 'TyArgs' from a list of (type, source-name) pairs.
toTyArgs :: [(Type, Name)] -> TyArgs
toTyArgs []       = NoTyArgs
toTyArgs (x : xs) = SomeTyArgs (x :| xs)

-- | The type arguments as a plain list (empty for 'NoTyArgs').
tyArgsList :: TyArgs -> [(Type, Name)]
tyArgsList NoTyArgs        = []
tyArgsList (SomeTyArgs ne) = NonEmpty.toList ne

-- | Just the 'Type's of the arguments, in order.
tyArgsTypes :: TyArgs -> [Type]
tyArgsTypes = map fst . tyArgsList

-- | Just the source 'Name's of the arguments, in order.
tyArgsBinders :: TyArgs -> [Name]
tyArgsBinders = map snd . tyArgsList

-- | How many type arguments there are.
tyArgsArity :: TyArgs -> Int
tyArgsArity NoTyArgs        = 0
tyArgsArity (SomeTyArgs ne) = NonEmpty.length ne

-- | Whether there are any type arguments (i.e. the site is parameterized).
hasTyArgs :: TyArgs -> Bool
hasTyArgs NoTyArgs      = False
hasTyArgs SomeTyArgs {} = True

-- | Apply the type arguments to a head type (e.g. a route\/subroute type
-- constructor), left to right. This replaces the ad-hoc
-- @'foldl'' 'AppT' con ('fst' '<$>' tyargs)@ that recurred across the
-- generators.
applyTyArgs :: Type -> TyArgs -> Type
applyTyArgs t = foldl' AppT t . tyArgsTypes

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

data ParentDetails a = ParentDetails
    { pdName :: String
    , pdPieces :: [Piece a]
    , pdAttrs :: Set String
    } deriving (Show)

data FlatResource a = FlatResource
    { frParentDetails :: [ParentDetails a]
    , frName :: String
    , frPieces :: [Piece a]
    , frDispatch :: Dispatch a
    , frCheck :: Bool
    } deriving (Show)

flatten :: [ResourceTree a] -> [FlatResource a]
flatten =
    concatMap (go id True)
  where
    go front check' (ResourceLeaf (Resource a b c _ check)) =
        [FlatResource (front []) a b c (check' && check)]
    go front check' (ResourceParent name check attrs pieces children) =
        concatMap (go (front . ((ParentDetails name pieces attrs):)) (check && check')) children
