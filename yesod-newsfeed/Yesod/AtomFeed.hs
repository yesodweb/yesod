{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Text.Hamlet (hamlet)
import qualified Data.ByteString.Char8 as S8
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Text.XML
#if MIN_VERSION_blaze_html(0, 5, 0)
import Text.Blaze.Html.Renderer.Text (renderHtml)
#else
import Text.Blaze.Renderer.Text (renderHtml)
#endif

newtype RepAtom = RepAtom Content
instance HasReps RepAtom where
    chooseRep (RepAtom c) _ = return (typeAtom, c)

atomFeed :: Feed (Route master) -> GHandler sub master RepAtom
atomFeed feed = do
    render <- getUrlRender
    return $ RepAtom $ toContent $ renderLBS def $ template feed render

template :: Feed url -> (url -> Text) -> Document
template Feed {..} render =
    Document (Prologue [] Nothing []) (addNS root) []
  where
    addNS (Element (Name ln _ _) as ns) = Element (Name ln (Just namespace) Nothing) as (map addNS' ns)
    addNS' (NodeElement e) = NodeElement $ addNS e
    addNS' n = n
    namespace = "http://www.w3.org/2005/Atom"

    root = Element "feed" [] $ map NodeElement
        $ Element "title" [] [NodeContent feedTitle]
        : Element "link" [("rel", "self"), ("href", render feedLinkSelf)] []
        : Element "link" [("href", render feedLinkHome)] []
        : Element "updated" [] [NodeContent $ formatW3 feedUpdated]
        : Element "id" [] [NodeContent $ render feedLinkHome]
        : map (flip entryTemplate render) feedEntries

entryTemplate :: FeedEntry url -> (url -> Text) -> Element
entryTemplate FeedEntry {..} render = Element "entry" [] $ map NodeElement
    [ Element "id" [] [NodeContent $ render feedEntryLink]
    , Element "link" [("href", render feedEntryLink)] []
    , Element "updated" [] [NodeContent $ formatW3 feedEntryUpdated]
    , Element "title" [] [NodeContent feedEntryTitle]
    , Element "content" [("type", "html")] [NodeContent $ toStrict $ renderHtml feedEntryContent]
    ]

-- | Generates a link tag in the head of a widget.
atomLink :: Route m
         -> Text -- ^ title
         -> GWidget s m ()
atomLink r title = toWidgetHead [hamlet|
<link href=@{r} type=#{S8.unpack typeAtom} rel="alternate" title=#{title}>
|]
