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
    , newsFeedText
    , module Yesod.FeedTypes
    ) where

import Yesod.FeedTypes
import Yesod.AtomFeed
import Yesod.RssFeed
import Yesod.Core

import Data.Text

newsFeed :: MonadHandler m => Feed (Route (HandlerSite m)) -> m TypedContent
newsFeed f = selectRep $ do
    provideRep $ atomFeed f
    provideRep $ rssFeed f

-- | Same as @'newsFeed'@ but for @'Feed Text'@. Useful for cases where you are
--   generating a feed of external links.
newsFeedText :: MonadHandler m => Feed Text -> m TypedContent
newsFeedText f = selectRep $ do
    provideRep $ atomFeedText f
    provideRep $ rssFeedText f
