module Yesod.FeedTypes
    ( Feed (..)
    , FeedEntry (..)
    ) where

import Text.Hamlet      (Html)
import Data.Time.Clock  (UTCTime)
import Data.Text        (Text)

-- | The overall feed
data Feed url = Feed
    { feedTitle       :: Text
    , feedLinkSelf    :: url
    , feedLinkHome    :: url
    , feedAuthor      :: Text


    -- | note: currently only used for Rss
    , feedDescription :: Html

    -- | note: currently only used for Rss, possible values: 
    --   <http://www.rssboard.org/rss-language-codes>
    , feedLanguage    :: Text

    , feedUpdated     :: UTCTime
    , feedLogo        :: Maybe (url, Text)
    , feedEntries     :: [FeedEntry url]
    }

-- | Each feed entry
data FeedEntry url = FeedEntry
    { feedEntryLink    :: url
    , feedEntryUpdated :: UTCTime
    , feedEntryTitle   :: Text
    , feedEntryContent :: Html
    , feedEntryEnclosure :: Maybe (url, Int, Text)
    }
