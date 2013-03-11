-------------------------------------------------------------------------------
--
-- Module        : Yesod.Feed
-- Copyright     : Patrick Brisbin
-- License       : as-is
--
-- Maintainer    : Patrick Brisbin <me@pbrisbin.com>
-- Stability     : Stable
-- Portability   : Portable
--
-- Generic Feed and Feed Entry data types that can be used as either an
-- Rss feed or an Atom feed (or both, or other).
--
-- Atom spec: <http://en.wikipedia.org/wiki/Atom_(standard)>
-- Rss spec:  <http://www.rssboard.org/rss-specification>
--
-------------------------------------------------------------------------------
module Yesod.Feed
    ( newsFeed
    , module Yesod.FeedTypes
    ) where

import Yesod.FeedTypes
import Yesod.AtomFeed
import Yesod.RssFeed
import Yesod.Core

newsFeed :: Feed (Route master) -> GHandler sub master TypedContent
newsFeed f = selectRep $ do
    provideRep $ atomFeed f
    provideRep $ rssFeed f
