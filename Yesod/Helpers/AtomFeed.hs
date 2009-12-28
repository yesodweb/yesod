{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
---------------------------------------------------------
--
-- Module        : Yesod.Helpers.AtomFeed
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
-- Generating atom news feeds.
--
---------------------------------------------------------

module Yesod.Helpers.AtomFeed
    ( AtomFeed (..)
    , AtomFeedEntry (..)
    , AtomFeedResponse (..)
    , atomFeed
    ) where

import Yesod
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL

import Data.Time.Clock
import Web.Encodings

data AtomFeedResponse = AtomFeedResponse AtomFeed Approot

atomFeed :: YesodApproot y => AtomFeed -> Handler y AtomFeedResponse
atomFeed f = do
    y <- getYesod
    return $ AtomFeedResponse f $ approot y

data AtomFeed = AtomFeed
    { atomTitle :: String
    , atomLinkSelf :: Location
    , atomLinkHome :: Location
    , atomUpdated :: UTCTime
    , atomEntries :: [AtomFeedEntry]
    }
instance HasReps AtomFeedResponse where
    reps =
        [ (TypeAtom, return . cs)
        ]

data AtomFeedEntry = AtomFeedEntry
    { atomEntryLink :: Location
    , atomEntryUpdated :: UTCTime
    , atomEntryTitle :: String
    , atomEntryContent :: Html
    }

instance ConvertSuccess AtomFeedResponse Content where
    convertSuccess = (cs :: Text -> Content) . cs
instance ConvertSuccess AtomFeedResponse Text where
    convertSuccess (AtomFeedResponse f ar) = TL.concat
        [ cs "<?xml version='1.0' encoding='utf-8' ?>\n"
        , cs "<feed xmlns='http://www.w3.org/2005/Atom'>"
        , cs "<title>"
        , encodeHtml $ cs $ atomTitle f
        , cs "</title>"
        , cs "<link rel='self' href='"
        , encodeHtml $ cs $ showLocation ar $ atomLinkSelf f
        , cs "'/>"
        , cs "<link href='"
        , encodeHtml $ cs $ showLocation ar $ atomLinkHome f
        , cs "'/>"
        , cs "<updated>"
        , cs $ formatW3 $ atomUpdated f
        , cs "</updated>"
        , cs "<id>"
        , encodeHtml $ cs $ showLocation ar $ atomLinkHome f
        , cs "</id>"
        , TL.concat $ map cs $ zip (atomEntries f) $ repeat ar
        , cs "</feed>"
        ]

instance ConvertSuccess (AtomFeedEntry, Approot) Text where
    convertSuccess (e, ar) = TL.concat
        [ cs "<entry>"
        , cs "<id>"
        , encodeHtml $ cs $ showLocation ar $ atomEntryLink e
        , cs "</id>"
        , cs "<link href='"
        , encodeHtml $ cs $ showLocation ar $ atomEntryLink e
        , cs "' />"
        , cs "<updated>"
        , cs $ formatW3 $ atomEntryUpdated e
        , cs "</updated>"
        , cs "<title>"
        , encodeHtml $ cs $ atomEntryTitle e
        , cs "</title>"
        , cs "<content type='html'><![CDATA["
        , cs $ atomEntryContent e
        , cs "]]></content>"
        , cs "</entry>"
        ]
