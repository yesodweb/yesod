{-# LANGUAGE QuasiQuotes #-}
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

-- | Generation of Atom newsfeeds. See
-- <http://en.wikipedia.org/wiki/Atom_(standard)>.
module Yesod.Helpers.AtomFeed
    ( AtomFeed (..)
    , AtomFeedEntry (..)
    , atomFeed
    , RepAtom (..)
    ) where

import Yesod
import Data.Time.Clock (UTCTime)
import Web.Encodings (formatW3)
import Text.Hamlet.Monad

newtype RepAtom = RepAtom Content
instance HasReps RepAtom where
    chooseRep (RepAtom c) _ = return (TypeAtom, c)

atomFeed :: AtomFeed (Routes master) -> GHandler sub master RepAtom
atomFeed = fmap RepAtom . hamletToContent . template

data AtomFeed url = AtomFeed
    { atomTitle :: String
    , atomLinkSelf :: url
    , atomLinkHome :: url
    , atomUpdated :: UTCTime
    , atomEntries :: [AtomFeedEntry url]
    }

data AtomFeedEntry url = AtomFeedEntry
    { atomEntryLink :: url
    , atomEntryUpdated :: UTCTime
    , atomEntryTitle :: String
    , atomEntryContent :: HtmlContent
    }

xmlns :: AtomFeed url -> HtmlContent
xmlns _ = cs "http://www.w3.org/2005/Atom"

template :: AtomFeed url -> Hamlet url IO ()
template arg = [$hamlet|
%feed!xmlns=$xmlns.arg$
    %title $cs.atomTitle.arg$
    %link!rel=self!href=@atomLinkSelf.arg@
    %link!href=@atomLinkHome.arg@
    %updated $cs.formatW3.atomUpdated.arg$
    %id @atomLinkHome.arg@
    $forall atomEntries.arg entry
        ^entryTemplate.entry^
|]

entryTemplate :: AtomFeedEntry url -> Hamlet url IO ()
entryTemplate arg = [$hamlet|
%entry
    %id @atomEntryLink.arg@
    %link!href=@atomEntryLink.arg@
    %updated $cs.formatW3.atomEntryUpdated.arg$
    %title $cs.atomEntryTitle.arg$
    %content!type=html $cdata.atomEntryContent.arg$
|]
