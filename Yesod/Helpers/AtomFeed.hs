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
    ) where

import Yesod
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL

import Data.Time.Clock
import Web.Encodings

data AtomFeed = AtomFeed
    { atomTitle :: String
    , atomLinkSelf :: String
    , atomLinkHome :: String
    , atomUpdated :: UTCTime
    , atomEntries :: [AtomFeedEntry]
    }
instance HasReps AtomFeed where
    reps =
        [ (TypeAtom, return . cs)
        ]

data AtomFeedEntry = AtomFeedEntry
    { atomEntryLink :: String
    , atomEntryUpdated :: UTCTime
    , atomEntryTitle :: String
    , atomEntryContent :: Html
    }

instance ConvertSuccess AtomFeed Content where
    convertSuccess = cs . (cs :: AtomFeed -> Text)
instance ConvertSuccess AtomFeed Text where
    convertSuccess f = TL.concat
        [ cs "<?xml version='1.0' encoding='utf-8' ?>\n"
        , cs "<feed xmlns='http://www.w3.org/2005/Atom'>"
        , cs "<title>"
        , encodeHtml $ cs $ atomTitle f
        , cs "</title>"
        , cs "<link rel='self' href='"
        , encodeHtml $ cs $ atomLinkSelf f
        , cs "'/>"
        , cs "<link href='"
        , encodeHtml $ cs $ atomLinkHome f
        , cs "'/>"
        , cs "<updated>"
        , cs $ formatW3 $ atomUpdated f
        , cs "</updated>"
        , cs "<id>"
        , encodeHtml $ cs $ atomLinkHome f
        , cs "</id>"
        , TL.concat $ map cs $ atomEntries f
        , cs "</feed>"
        ]

instance ConvertSuccess AtomFeedEntry Text where
    convertSuccess e = TL.concat
        [ cs "<entry>"
        , cs "<id>"
        , encodeHtml $ cs $ atomEntryLink e
        , cs "</id>"
        , cs "<link href='"
        , encodeHtml $ cs $ atomEntryLink e
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
