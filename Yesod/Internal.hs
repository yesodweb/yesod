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
      -- * UTF8 helpers
    , bsToChars
    , lbsToChars
    , charsToBs
      -- * Names
    , sessionName
    , nonceKey
    ) where

import Text.Hamlet (Hamlet, hamlet, Html)
import Text.Cassius (Cassius)
import Text.Julius (Julius)
import Data.Monoid (Monoid (..), Last)
import Data.List (nub)

import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T

import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT

import qualified Network.Wai as W
import Data.Typeable (Typeable)
import Control.Exception (Exception)

#if GHC7
#define HAMLET hamlet
#else
#define HAMLET $hamlet
#endif

-- | Responses to indicate some form of an error occurred. These are different
-- from 'SpecialResponse' in that they allow for custom error pages.
data ErrorResponse =
      NotFound
    | InternalError String
    | InvalidArgs [String]
    | PermissionDenied String
    | BadMethod String
    deriving (Show, Eq, Typeable)
instance Exception ErrorResponse

----- header stuff
-- | Headers to be added to a 'Result'.
data Header =
    AddCookie Int ByteString ByteString
    | DeleteCookie ByteString
    | Header W.ResponseHeader ByteString
    deriving (Eq, Show)

langKey :: String
langKey = "_LANG"

data Location url = Local url | Remote String
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

newtype Script url = Script { unScript :: Location url }
    deriving (Show, Eq)
newtype Stylesheet url = Stylesheet { unStylesheet :: Location url }
    deriving (Show, Eq)
newtype Title = Title { unTitle :: Html }

newtype Head url = Head (Hamlet url)
    deriving Monoid
newtype Body url = Body (Hamlet url)
    deriving Monoid

lbsToChars :: L.ByteString -> String
lbsToChars = LT.unpack . LT.decodeUtf8With T.lenientDecode

bsToChars :: S.ByteString -> String
bsToChars = T.unpack . T.decodeUtf8With T.lenientDecode

charsToBs :: String -> S.ByteString
charsToBs = T.encodeUtf8 . T.pack

nonceKey :: String
nonceKey = "_NONCE"

sessionName :: ByteString
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
