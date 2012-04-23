{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
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
#if MIN_VERSION_blaze_html(0, 5, 0)
import Text.Blaze.Html.Renderer.Text (renderHtml)
#else
import Text.Blaze.Renderer.Text (renderHtml)
#endif

newtype RepRss = RepRss Content
instance HasReps RepRss where
    chooseRep (RepRss c) _ = return (typeRss, c)

-- | Generate the feed
rssFeed :: Feed (Route master) -> GHandler sub master RepRss
rssFeed feed = do
    render <- getUrlRender
    return $ RepRss $ toContent $ renderLBS def $ template feed render

template :: Feed url -> (url -> Text) -> Document
template Feed {..} render =
    Document (Prologue [] Nothing []) root []
  where
    root = Element "rss" [("version", "2.0")] $ return $ NodeElement $ Element "channel" [] $ map NodeElement
        $ Element "{http://www.w3.org/2005/Atom}link"
            [ ("href", render feedLinkSelf)
            , ("rel", "self")
            , ("type", pack $ S8.unpack typeRss)
            ] []
        : Element "title" [] [NodeContent feedTitle]
        : Element "link" [] [NodeContent $ render feedLinkHome]
        : Element "description" [] [NodeContent $ toStrict $ renderHtml feedDescription]
        : Element "lastBuildDate" [] [NodeContent $ formatRFC822 feedUpdated]
        : Element "language" [] [NodeContent feedLanguage]
        : map (flip entryTemplate render) feedEntries

entryTemplate :: FeedEntry url -> (url -> Text) -> Element
entryTemplate FeedEntry {..} render = Element "item" [] $ map NodeElement
    [ Element "title" [] [NodeContent feedEntryTitle]
    , Element "link" [] [NodeContent $ render feedEntryLink]
    , Element "guid" [] [NodeContent $ render feedEntryLink]
    , Element "pubDate" [] [NodeContent $ formatRFC822 feedEntryUpdated]
    , Element "description" [] [NodeContent $ toStrict $ renderHtml feedEntryContent]
    ]

-- | Generates a link tag in the head of a widget.
rssLink :: Route m
        -> Text -- ^ title
        -> GWidget s m ()
rssLink r title = toWidgetHead [hamlet|
    <link href=@{r} type=#{S8.unpack typeRss} rel="alternate" title=#{title}>
    |]
