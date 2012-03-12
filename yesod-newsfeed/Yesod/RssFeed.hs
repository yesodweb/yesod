{-# LANGUAGE QuasiQuotes #-}
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
import Text.Hamlet (HtmlUrl, xhamlet, hamlet)
import qualified Data.ByteString.Char8 as S8
import Control.Monad (liftM)
import Data.Text (Text)

newtype RepRss = RepRss Content
instance HasReps RepRss where
    chooseRep (RepRss c) _ = return (typeRss, c)

-- | Generate the feed
rssFeed :: Feed (Route master) -> GHandler sub master RepRss
rssFeed = liftM RepRss . hamletToContent . template

template :: Feed url -> HtmlUrl url
template arg = [xhamlet|
    \<?xml version="1.0" encoding="utf-8"?> 
    <rss version="2.0" xmlns:atom="http://www.w3.org/2005/Atom"
        <channel
            <atom:link href=@{feedLinkSelf arg} rel="self" type=#{S8.unpack typeRss}
            <title>        #{feedTitle arg}
            <link>         @{feedLinkHome arg}
            <description>  #{feedDescription arg}
            <lastBuildDate>#{formatRFC822 $ feedUpdated arg}
            <language>     #{feedLanguage arg}

            $forall entry <- feedEntries arg
                ^{entryTemplate entry}
    |]

entryTemplate :: FeedEntry url -> HtmlUrl url
entryTemplate arg = [xhamlet|
    <item
        <title>      #{feedEntryTitle arg}
        <link>       @{feedEntryLink arg}
        <guid>       @{feedEntryLink arg}
        <pubDate>    #{formatRFC822 $ feedEntryUpdated arg}
        <description>#{feedEntryContent arg}
    |]

-- | Generates a link tag in the head of a widget.
rssLink :: Route m
        -> Text -- ^ title
        -> GWidget s m ()
rssLink r title = addHamletHead [hamlet|
    <link href=@{r} type=#{S8.unpack typeRss} rel="alternate" title=#{title}
    |]
