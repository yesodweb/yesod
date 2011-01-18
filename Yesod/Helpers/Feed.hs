-------------------------------------------------------------------------------
--
-- Module        : Yesod.Helpers.Feed
-- Copyright     : Patrick Brisbin
-- License       : as-is
--
-- Maintainer    : Patrick Brisbin <me@pbrisbin.com>
-- Stability     : Stable
-- Portability   : Portable
--
-- Generic Feed and Feed Entry data types that can be used as either and
-- Rss feed or an Atom feed (or both, or other).
--
-------------------------------------------------------------------------------
module Yesod.Helpers.Feed
    ( Feed(..)
    , FeedEntry(..)
    ) where

import Text.Hamlet      (Html)
import Data.Time.Clock  (UTCTime)

-- | The overal feed
data Feed url = Feed
    { feedTitle       :: String
    , feedLinkSelf    :: url
    , feedLinkHome    :: url
    , feedDescription :: Html
    , feedLanguage    :: String
    , feedUpdated     :: UTCTime
    , feedEntries     :: [FeedEntry url]
    }

-- | Each feed entry
data FeedEntry url = FeedEntry
    { feedEntryLink    :: url
    , feedEntryUpdated :: UTCTime
    , feedEntryTitle   :: String
    , feedEntryContent :: Html
    }
