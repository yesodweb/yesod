module Yesod.Helpers.FeedTypes
    ( Feed (..)
    , FeedEntry (..)
    ) where

import Text.Hamlet      (Html)
import Data.Time.Clock  (UTCTime)
import Data.Text        (Text)

-- | The overal feed
data Feed url = Feed
    { feedTitle       :: Text
    , feedLinkSelf    :: url
    , feedLinkHome    :: url


    -- | note: currently only used for Rss
    , feedDescription :: Html

    -- | note: currently only used for Rss, possible values: 
    --   <http://www.rssboard.org/rss-language-codes>
    , feedLanguage    :: Text

    , feedUpdated     :: UTCTime
    , feedEntries     :: [FeedEntry url]
    }

-- | Each feed entry
data FeedEntry url = FeedEntry
    { feedEntryLink    :: url
    , feedEntryUpdated :: UTCTime
    , feedEntryTitle   :: Text
    , feedEntryContent :: Html
    }
