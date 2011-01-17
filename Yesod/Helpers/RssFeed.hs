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
    ( RssFeed (..)
    , RssFeedEntry (..)
    , rssFeed
    , rssLink
    , RepRss (..)
    ) where

import Yesod.Handler
import Yesod.Content
import Yesod.Widget
import Text.Hamlet
import Data.Time.Clock  (UTCTime)

newtype RepRss = RepRss Content
instance HasReps RepRss where
    chooseRep (RepRss c) _ = return (typeRss, c)

-- | Generate the feed
rssFeed :: RssFeed (Route master) -> GHandler sub master RepRss
rssFeed = fmap RepRss . hamletToContent . template

-- | Data type for the overall feed
data RssFeed url = RssFeed
    { rssTitle       :: String
    , rssLinkSelf    :: url
    , rssLinkHome    :: url
    , rssDescription :: String
    , rssLanguage    :: String
    , rssUpdated     :: UTCTime
    , rssEntries     :: [RssFeedEntry url]
    }

-- | Data type for each feed entry
data RssFeedEntry url = RssFeedEntry
    { rssEntryLink    :: url
    , rssEntryUpdated :: UTCTime
    , rssEntryTitle   :: String
    , rssEntryContent :: Html
    }

template :: RssFeed url -> Hamlet url
template arg = 
#if __GLASGOW_HASKELL__ >= 700
    [xhamlet|
#else
    [$xhamlet|
#endif
    %rss!version="2.0"!xmlns:atom="http://www.w3.org/2005/Atom"

        %channel
            %atom:link!href=@rssLinkSelf.arg@!rel="self"!type=$typeRss$
            %title         $rssTitle.arg$
            %link          @rssLinkHome.arg@
            %description   $rssDescription.arg$
            %lastBuildDate $formatRFC822.rssUpdated.arg$
            %language      $rssLanguage.arg$

            $forall rssEntries.arg entry
                ^entryTemplate.entry^
    |]

entryTemplate :: RssFeedEntry url -> Hamlet url
entryTemplate arg = 
#if __GLASGOW_HASKELL__ >= 700
    [xhamlet|
#else
    [$xhamlet|
#endif
    %item
        %title       $rssEntryTitle.arg$
        %link        @rssEntryLink.arg@
        %guid        @rssEntryLink.arg@
        %pubDate     $formatRFC822.rssEntryUpdated.arg$
        %description $rssEntryContent.arg$
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
