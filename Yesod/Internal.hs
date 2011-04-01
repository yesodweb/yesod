{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | Normal users should never need access to these.
module Yesod.Internal
    ( -- * Error responses
      ErrorResponse (..)
      -- * Header
    , Header (..)
      -- * Cookie names
    , langKey
      -- * Widgets
    , GWData (..)
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
      -- * Names
    , sessionName
    , nonceKey
    ) where

import Text.Hamlet (Hamlet, hamlet, Html)
import Text.Cassius (Cassius)
import Text.Julius (Julius)
import Data.Monoid (Monoid (..), Last)
import Data.List (nub)

import Data.Text (Text)

import Data.Typeable (Typeable)
import Control.Exception (Exception)

import qualified Network.HTTP.Types as H
import qualified Network.HTTP.Types as A
import Data.CaseInsensitive (CI)
import Data.String (IsString)

#if GHC7
#define HAMLET hamlet
#else
#define HAMLET $hamlet
#endif

-- | Responses to indicate some form of an error occurred. These are different
-- from 'SpecialResponse' in that they allow for custom error pages.
data ErrorResponse =
      NotFound
    | InternalError Text
    | InvalidArgs [Text]
    | PermissionDenied Text
    | BadMethod H.Method
    deriving (Show, Eq, Typeable)
instance Exception ErrorResponse

----- header stuff
-- | Headers to be added to a 'Result'.
data Header =
    AddCookie Int A.Ascii A.Ascii
    | DeleteCookie A.Ascii
    | Header (CI A.Ascii) A.Ascii
    deriving (Eq, Show)

langKey :: IsString a => a
langKey = "_LANG"

data Location url = Local url | Remote Text
    deriving (Show, Eq)
locationToHamlet :: Location url -> Hamlet url
locationToHamlet (Local url) = [HAMLET|\@{url}
|]
locationToHamlet (Remote s) = [HAMLET|\#{s}
|]

newtype UniqueList x = UniqueList ([x] -> [x])
instance Monoid (UniqueList x) where
    mempty = UniqueList id
    UniqueList x `mappend` UniqueList y = UniqueList $ x . y
runUniqueList :: Eq x => UniqueList x -> [x]
runUniqueList (UniqueList x) = nub $ x []
toUnique :: x -> UniqueList x
toUnique = UniqueList . (:)

data Script url = Script { scriptLocation :: Location url, scriptAttributes :: [(Text, Text)] }
    deriving (Show, Eq)
data Stylesheet url = Stylesheet { styleLocation :: Location url, styleAttributes :: [(Text, Text)] }
    deriving (Show, Eq)
newtype Title = Title { unTitle :: Html }

newtype Head url = Head (Hamlet url)
    deriving Monoid
newtype Body url = Body (Hamlet url)
    deriving Monoid

nonceKey :: IsString a => a
nonceKey = "_NONCE"

sessionName :: IsString a => a
sessionName = "_SESSION"

data GWData a = GWData
    !(Body a)
    !(Last Title)
    !(UniqueList (Script a))
    !(UniqueList (Stylesheet a))
    !(Maybe (Cassius a))
    !(Maybe (Julius a))
    !(Head a)
instance Monoid (GWData a) where
    mempty = GWData mempty mempty mempty mempty mempty mempty mempty
    mappend (GWData a1 a2 a3 a4 a5 a6 a7)
            (GWData b1 b2 b3 b4 b5 b6 b7) = GWData
        (a1 `mappend` b1)
        (a2 `mappend` b2)
        (a3 `mappend` b3)
        (a4 `mappend` b4)
        (a5 `mappend` b5)
        (a6 `mappend` b6)
        (a7 `mappend` b7)
