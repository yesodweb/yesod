module Yesod.Routes.TH.Types
    ( -- * Data types
      Resource (..)
    , Piece (..)
    , Dispatch (..)
      -- ** Helper functions
    , resourceMulti
    ) where

import Language.Haskell.TH.Syntax

data Resource = Resource
    { resourceName :: String
    , resourcePieces :: [Piece]
    , resourceDispatch :: Dispatch
    }

data Piece = Static String | Dynamic Type

data Dispatch =
    Methods
        { methodsMulti :: Maybe Type -- ^ type of the multi piece at the end
        , methodsMethods :: [String] -- ^ supported request methods
        }
    | Subsite
        { subsiteType :: Type
        , subsiteFunc :: String
        }

resourceMulti :: Resource -> Maybe Type
resourceMulti Resource { resourceDispatch = Methods (Just t) _ } = Just t
resourceMulti _ = Nothing
