{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | Normal users should never need access to these.
--
-- Note that no guarantees of API stability are provided on this module. Use at your own risk.
module Yesod.Internal
    ( -- * Error responses
      ErrorResponse (..)
    , HandlerContents (..)
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
    , locationToHtmlUrl
    , runUniqueList
    , toUnique
      -- * Names
    , tokenKey
    ) where

import Text.Hamlet (HtmlUrl, Html)
import Text.Blaze.Html (toHtml)
import Text.Julius (JavascriptUrl)
import Data.Monoid (Monoid (..), Last)
import Data.List (nub)

import Data.Text (Text)

import Data.Typeable (Typeable)
import Control.Exception (Exception)

import qualified Network.HTTP.Types as H
import Data.String (IsString)
import qualified Data.Map as Map
import Data.Text.Lazy.Builder (Builder)
import Web.Cookie (SetCookie (..))
import Data.ByteString (ByteString)
import qualified Network.Wai as W
import Yesod.Content (ChooseRep, ContentType)

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
    AddCookie SetCookie
    | DeleteCookie ByteString ByteString
    | Header ByteString ByteString
    deriving (Eq, Show)

langKey :: IsString a => a
langKey = "_LANG"

data Location url = Local url | Remote Text
    deriving (Show, Eq)
locationToHtmlUrl :: Location url -> HtmlUrl url
locationToHtmlUrl (Local url) render = toHtml $ render url []
locationToHtmlUrl (Remote s) _ = toHtml s

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

newtype Head url = Head (HtmlUrl url)
    deriving Monoid
newtype Body url = Body (HtmlUrl url)
    deriving Monoid

tokenKey :: IsString a => a
tokenKey = "_TOKEN"

type CssBuilderUrl a = (a -> [(Text, Text)] -> Text) -> Builder

data GWData a = GWData
    { gwdBody :: !(Body a)
    , gwdTitle :: !(Last Title)
    , gwdScripts :: !(UniqueList (Script a))
    , gwdStylesheets :: !(UniqueList (Stylesheet a))
    , gwdCss :: !(Map.Map (Maybe Text) (CssBuilderUrl a)) -- media type
    , gwdJavascript :: !(Maybe (JavascriptUrl a))
    , gwdHead :: !(Head a)
    }
instance Monoid (GWData a) where
    mempty = GWData mempty mempty mempty mempty mempty mempty mempty
    mappend (GWData a1 a2 a3 a4 a5 a6 a7)
            (GWData b1 b2 b3 b4 b5 b6 b7) = GWData
        (a1 `mappend` b1)
        (a2 `mappend` b2)
        (a3 `mappend` b3)
        (a4 `mappend` b4)
        (Map.unionWith mappend a5 b5)
        (a6 `mappend` b6)
        (a7 `mappend` b7)

data HandlerContents =
      HCContent H.Status ChooseRep
    | HCError ErrorResponse
    | HCSendFile ContentType FilePath (Maybe W.FilePart)
    | HCRedirect H.Status Text
    | HCCreated Text
    | HCWai W.Response
    deriving Typeable

instance Show HandlerContents where
    show _ = "Cannot show a HandlerContents"
instance Exception HandlerContents
