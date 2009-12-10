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

import Yesod.Response

import Data.Time.Clock
import Web.Encodings

data AtomFeed = AtomFeed
    { atomTitle :: String
    , atomLinkSelf :: String
    , atomLinkHome :: String
    , atomUpdated :: UTCTime
    , atomEntries :: [AtomFeedEntry]
    }
instance Monad m => HasReps AtomFeed m where
    reps e =
        [ ("application/atom+xml", return $ toContent $ show e)
        ]

data AtomFeedEntry = AtomFeedEntry
    { atomEntryLink :: String
    , atomEntryUpdated :: UTCTime
    , atomEntryTitle :: String
    , atomEntryContent :: String
    }

instance Show AtomFeed where
    show f = concat
        [ "<?xml version='1.0' encoding='utf-8' ?>\n"
        , "<feed xmlns='http://www.w3.org/2005/Atom'>"
        , "<title>"
        , encodeHtml $ atomTitle f
        , "</title>"
        , "<link rel='self' href='"
        , encodeHtml $ atomLinkSelf f
        , "'/>"
        , "<link href='"
        , encodeHtml $ atomLinkHome f
        , "'/>"
        , "<updated>"
        , formatW3 $ atomUpdated f
        , "</updated>"
        , "<id>"
        , encodeHtml $ atomLinkHome f
        , "</id>"
        , concatMap show $ atomEntries f
        , "</feed>"
        ]

instance Show AtomFeedEntry where
    show e = concat
        [ "<entry>"
        , "<id>"
        , encodeHtml $ atomEntryLink e
        , "</id>"
        , "<link href='"
        , encodeHtml $ atomEntryLink e
        , "' />"
        , "<updated>"
        , formatW3 $ atomEntryUpdated e
        , "</updated>"
        , "<title>"
        , encodeHtml $ atomEntryTitle e
        , "</title>"
        , "<content type='html'><![CDATA["
        , atomEntryContent e
        , "]]></content>"
        , "</entry>"
        ]
