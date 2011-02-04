module Yesod.Helpers.FeedTypes
    ( Feed (..)
    , FeedEntry (..)
    ) where

import Text.Hamlet      (Html)
import Data.Time.Clock  (UTCTime)

-- | The overal feed
data Feed url = Feed
    { feedTitle       :: String
    , feedLinkSelf    :: url
    , feedLinkHome    :: url


    -- | note: currently only used for Rss
    , feedDescription :: Html

    -- | note: currently only used for Rss, possible values: 
    --   <http://www.rssboard.org/rss-language-codes>
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
