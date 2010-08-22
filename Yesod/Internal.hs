{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Normal users should never need access to these.
module Yesod.Internal
    ( -- * Error responses
      ErrorResponse (..)
      -- * Header
    , Header (..)
      -- * Cookie names
    , langKey
      -- * Widgets
    , Location (..)
    , UniqueList (..)
    , Script (..)
    , Stylesheet (..)
    , Title (..)
    , Head (..)
    , Body (..)
    , locationToHamlet
    , runUniqueList
    , toUnique
    ) where

import Text.Hamlet (Hamlet, hamlet, Html)
import Data.Monoid (Monoid (..))
import Data.List (nub)

-- | Responses to indicate some form of an error occurred. These are different
-- from 'SpecialResponse' in that they allow for custom error pages.
data ErrorResponse =
      NotFound
    | InternalError String
    | InvalidArgs [String]
    | PermissionDenied String
    | BadMethod String
    deriving (Show, Eq)

----- header stuff
-- | Headers to be added to a 'Result'.
data Header =
    AddCookie Int String String
    | DeleteCookie String
    | Header String String
    deriving (Eq, Show)

langKey :: String
langKey = "_LANG"

data Location url = Local url | Remote String
    deriving (Show, Eq)
locationToHamlet :: Location url -> Hamlet url
locationToHamlet (Local url) = [$hamlet|@url@|]
locationToHamlet (Remote s) = [$hamlet|$s$|]

newtype UniqueList x = UniqueList ([x] -> [x])
instance Monoid (UniqueList x) where
    mempty = UniqueList id
    UniqueList x `mappend` UniqueList y = UniqueList $ x . y
runUniqueList :: Eq x => UniqueList x -> [x]
runUniqueList (UniqueList x) = nub $ x []
toUnique :: x -> UniqueList x
toUnique = UniqueList . (:)

newtype Script url = Script { unScript :: Location url }
    deriving (Show, Eq)
newtype Stylesheet url = Stylesheet { unStylesheet :: Location url }
    deriving (Show, Eq)
newtype Title = Title { unTitle :: Html }

newtype Head url = Head (Hamlet url)
    deriving Monoid
newtype Body url = Body (Hamlet url)
    deriving Monoid
