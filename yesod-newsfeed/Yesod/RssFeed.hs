{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-------------------------------------------------------------------------------
--
-- Module        : Yesod.RssFeed
-- Copyright     : Patrick Brisbin
-- License       : as-is
--
-- Maintainer    : Patrick Brisbin <me@pbrisbin.com>
-- Stability     : Stable
-- Portability   : Portable
--
-------------------------------------------------------------------------------
module Yesod.RssFeed
    ( rssFeed
    , rssLink
    , RepRss (..)
    , module Yesod.FeedTypes
    ) where

import Yesod.Core
import Yesod.FeedTypes
import Text.Hamlet (hamlet)
import qualified Data.ByteString.Char8 as S8
import Data.Text (Text, pack)
import Data.Text.Lazy (toStrict)
import Text.XML
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Data.Map as Map

newtype RepRss = RepRss Content
    deriving ToContent
instance HasContentType RepRss where
    getContentType _ = typeRss
instance ToTypedContent RepRss where
    toTypedContent = TypedContent typeRss . toContent

-- | Generate the feed
rssFeed :: Feed (Route master) -> GHandler sub master RepRss
rssFeed feed = do
    render <- getUrlRender
    return $ RepRss $ toContent $ renderLBS def $ template feed render

template :: Feed url -> (url -> Text) -> Document
template Feed {..} render =
    Document (Prologue [] Nothing []) root []
  where
    root = Element "rss" (Map.singleton "version" "2.0") $ return $ NodeElement $ Element "channel" Map.empty $ map NodeElement
        $ Element "{http://www.w3.org/2005/Atom}link" (Map.fromList
            [ ("href", render feedLinkSelf)
            , ("rel", "self")
            , ("type", pack $ S8.unpack typeRss)
            ]) []
        : Element "title" Map.empty [NodeContent feedTitle]
        : Element "link" Map.empty [NodeContent $ render feedLinkHome]
        : Element "description" Map.empty [NodeContent $ toStrict $ renderHtml feedDescription]
        : Element "lastBuildDate" Map.empty [NodeContent $ formatRFC822 feedUpdated]
        : Element "language" Map.empty [NodeContent feedLanguage]
        : map (flip entryTemplate render) feedEntries

entryTemplate :: FeedEntry url -> (url -> Text) -> Element
entryTemplate FeedEntry {..} render = Element "item" Map.empty $ map NodeElement
    [ Element "title" Map.empty [NodeContent feedEntryTitle]
    , Element "link" Map.empty [NodeContent $ render feedEntryLink]
    , Element "guid" Map.empty [NodeContent $ render feedEntryLink]
    , Element "pubDate" Map.empty [NodeContent $ formatRFC822 feedEntryUpdated]
    , Element "description" Map.empty [NodeContent $ toStrict $ renderHtml feedEntryContent]
    ]

-- | Generates a link tag in the head of a widget.
rssLink :: Route m
        -> Text -- ^ title
        -> GWidget s m ()
rssLink r title = toWidgetHead [hamlet|
$newline never
    <link href=@{r} type=#{S8.unpack typeRss} rel="alternate" title=#{title}>
    |]
