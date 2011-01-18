{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}
-------------------------------------------------------------------------------
--
-- Module        : Yesod.Helpers.RssFeed
-- Copyright     : Patrick Brisbin
-- License       : as-is
--
-- Maintainer    : Patrick Brisbin <me@pbrisbin.com>
-- Stability     : Stable
-- Portability   : Portable
--
-------------------------------------------------------------------------------
module Yesod.Helpers.RssFeed
    ( rssFeed
    , rssLink
    , RepRss (..)
    , module Yesod.Helpers.Feed
    ) where

import Yesod.Handler
import Yesod.Content
import Yesod.Widget
import Yesod.Helpers.Feed

newtype RepRss = RepRss Content
instance HasReps RepRss where
    chooseRep (RepRss c) _ = return (typeRss, c)

-- | Generate the feed
rssFeed :: Feed (Route master) -> GHandler sub master RepRss
rssFeed = fmap RepRss . hamletToContent . template

template :: Feed url -> Hamlet url
template arg = 
#if __GLASGOW_HASKELL__ >= 700
    [xhamlet|
#else
    [$xhamlet|
#endif
    %rss!version="2.0"

        %channel
            %atom:link!href=@feedLinkSelf.arg@!rel="self"!type=$typeRss$
            %title         $feedTitle.arg$
            %link          @feedLinkHome.arg@
            %description   $feedDescription.arg$
            %lastBuildDate $formatRFC822.feedUpdated.arg$
            %language      $feedLanguage.arg$

            $forall feedEntries.arg entry
                ^entryTemplate.entry^
    |]

entryTemplate :: FeedEntry url -> Hamlet url
entryTemplate arg = 
#if __GLASGOW_HASKELL__ >= 700
    [xhamlet|
#else
    [$xhamlet|
#endif
    %item
        %title       $feedEntryTitle.arg$
        %link        @feedEntryLink.arg@
        %guid        @feedEntryLink.arg@
        %pubDate     $formatRFC822.feedEntryUpdated.arg$
        %description $feedEntryContent.arg$
    |]

-- | Generates a link tag in the head of a widget.
rssLink :: Route m
        -> String -- ^ title
        -> GWidget s m ()
rssLink u title = addHamletHead
#if __GLASGOW_HASKELL__ >= 700
    [hamlet|
#else
    [$hamlet|
#endif
    %link!href=@u@!type=$typeRss$!rel="alternate"!title=$title$
    |]
