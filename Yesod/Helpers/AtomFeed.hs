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

atomFeed :: AtomFeed (Routes sub) -> GHandler sub master RepAtom
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
template = [$hamlet|
%feed!xmlns=$xmlns$
    %title $atomTitle.cs$
    %link!rel=self!href=@atomLinkSelf@
    %link!href=@atomLinkHome@
    %updated $atomUpdated.formatW3.cs$
    %id @atomLinkHome@
    $forall atomEntries entry
        ^entry.entryTemplate^
|]

entryTemplate :: AtomFeedEntry url -> Hamlet url IO ()
entryTemplate = [$hamlet|
%entry
    %id @atomEntryLink@
    %link!href=@atomEntryLink@
    %updated $atomEntryUpdated.formatW3.cs$
    %title $atomEntryTitle.cs$
    %content!type=html $atomEntryContent.cdata$
|]
