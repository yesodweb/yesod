{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}
---------------------------------------------------------
--
-- Module        : Yesod.AtomFeed
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

-- | Generation of Atom newsfeeds.
module Yesod.AtomFeed
    ( atomFeed
    , atomLink
    , RepAtom (..)
    , module Yesod.FeedTypes
    ) where

import Yesod.Core
import Yesod.FeedTypes
import Text.Hamlet (HtmlUrl, xhamlet, hamlet)
import qualified Data.ByteString.Char8 as S8
import Control.Monad (liftM)

newtype RepAtom = RepAtom Content
instance HasReps RepAtom where
    chooseRep (RepAtom c) _ = return (typeAtom, c)

atomFeed :: Feed (Route master) -> GHandler sub master RepAtom
atomFeed = liftM RepAtom . hamletToContent . template

template :: Feed url -> HtmlUrl url
template arg =
#if __GLASGOW_HASKELL__ >= 700
                [xhamlet|
#else
                [$xhamlet|
#endif
\<?xml version="1.0" encoding="utf-8"?>
<feed xmlns="http://www.w3.org/2005/Atom"
    <title>#{feedTitle arg}
    <link rel=self href=@{feedLinkSelf arg}
    <link href=@{feedLinkHome arg}
    <updated>#{formatW3 $ feedUpdated arg}
    <id>@{feedLinkHome arg}
    $forall entry <- feedEntries arg
        ^{entryTemplate entry}
|]

entryTemplate :: FeedEntry url -> HtmlUrl url
entryTemplate arg =
#if __GLASGOW_HASKELL__ >= 700
                [xhamlet|
#else
                [$xhamlet|
#endif
<entry
    <id>@{feedEntryLink arg}
    <link href=@{feedEntryLink arg}
    <updated>#{formatW3 $ feedEntryUpdated arg}
    <title>#{feedEntryTitle arg}
    <content type=html>
        \<![CDATA[
        \#{feedEntryContent arg}
        ]]>
|]

-- | Generates a link tag in the head of a widget.
atomLink :: Route m
         -> String -- ^ title
         -> GWidget s m ()
atomLink u title = addHamletHead
#if __GLASGOW_HASKELL__ >= 700
                [hamlet|
#else
                [$hamlet|
#endif
<link href=@{u} type=#{S8.unpack typeAtom} rel="alternate" title=#{title}
|]
