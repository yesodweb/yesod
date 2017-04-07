module Yesod.FeedTypes
    ( Feed (..)
    , FeedEntry (..)
    , EntryEnclosure (..)
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

-- | RSS and Atom allow for linked content to be enclosed in a feed entry.
-- This represents the enclosed content.
--
-- Atom feeds ignore 'enclosedSize' and 'enclosedMimeType'.
--
-- @since 1.6
data EntryEnclosure url = EntryEnclosure
    { enclosedUrl :: url
    , enclosedSize :: Int -- ^ Specified in bytes
    , enclosedMimeType :: Text
    }

-- | Each feed entry
data FeedEntry url = FeedEntry
    { feedEntryLink    :: url
    , feedEntryUpdated :: UTCTime
    , feedEntryTitle   :: Text
    , feedEntryContent :: Html
    , feedEntryEnclosure :: Maybe (EntryEnclosure url)
      -- ^ Allows enclosed data: RSS \<enclosure> or Atom \<link
      -- rel=enclosure>
      --
      -- @since 1.5
    }
