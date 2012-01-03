{-# LANGUAGE TemplateHaskell #-}
module Yesod.Routes.TH.Types
    ( -- * Data types
      Resource (..)
    , Piece (..)
    , Dispatch (..)
      -- ** Helper functions
    , resourceMulti
    ) where

import Language.Haskell.TH.Syntax

liftOccName :: OccName -> Q Exp
liftOccName oc = [|mkOccName $(lift $ occString oc)|]

liftNameFlavour :: NameFlavour -> Q Exp
liftNameFlavour NameS = [|NameS|]

liftName :: Name -> Q Exp
liftName (Name a b) = [|Name $(liftOccName a) $(liftNameFlavour b)|]

liftType :: Type -> Q Exp
liftType (VarT name) = [|VarT $(liftName name)|]
liftType (ConT name) = [|ConT $(liftName name)|]
liftType (TupleT i) = [|TupleT $(lift i)|]
liftType ArrowT = [|ArrowT|]
liftType ListT = [|ListT|]
liftType (AppT a b) = [|AppT $(liftType a) $(liftType b)|]
liftType (SigT a b) = [|SigT $(liftType a) $(liftKind b)|]

liftKind :: Kind -> Q Exp
liftKind StarK = [|StarK|]
liftKind (ArrowK a b) = [|ArrowK $(liftKind a) $(liftKind b)|]

data Resource = Resource
    { resourceName :: String
    , resourcePieces :: [Piece]
    , resourceDispatch :: Dispatch
    }
    deriving Show

{-
instance Lift Resource where
    lift (Resource a b c) = [|Resource $(lift a) $(lift b) $(lift c)|]
-}

data Piece = Static String | Dynamic Type
    deriving Show

{-
instance Lift Piece where
    lift (Static s) = [|Static $(lift s)|]
    lift (Dynamic t) = [|Static $(liftType t)|]
-}

data Dispatch =
    Methods
        { methodsMulti :: Maybe Type -- ^ type of the multi piece at the end
        , methodsMethods :: [String] -- ^ supported request methods
        }
    | Subsite
        { subsiteType :: Type
        , subsiteFunc :: String
        }
    deriving Show

{-
instance Lift Dispatch where
    lift (Methods Nothing b) = [|Methods Nothing $(lift b)|]
    lift (Methods (Just t) b) = [|Methods (Just $(liftType t)) $(lift b)|]
    lift (Subsite t b) = [|Subsite $(liftType t) $(lift b)|]
-}

resourceMulti :: Resource -> Maybe Type
resourceMulti Resource { resourceDispatch = Methods (Just t) _ } = Just t
resourceMulti _ = Nothing
