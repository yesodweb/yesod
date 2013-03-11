{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Data.Map as Map

newtype RepAtom = RepAtom Content
    deriving ToContent
instance HasContentType RepAtom where
    getContentType _ = typeAtom
instance ToTypedContent RepAtom where
    toTypedContent = TypedContent typeAtom . toContent

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

    root = Element "feed" Map.empty $ map NodeElement
        $ Element "title" Map.empty [NodeContent feedTitle]
        : Element "link" (Map.fromList [("rel", "self"), ("href", render feedLinkSelf)]) []
        : Element "link" (Map.singleton "href" $ render feedLinkHome) []
        : Element "updated" Map.empty [NodeContent $ formatW3 feedUpdated]
        : Element "id" Map.empty [NodeContent $ render feedLinkHome]
        : Element "author" Map.empty [NodeContent feedAuthor]
        : map (flip entryTemplate render) feedEntries

entryTemplate :: FeedEntry url -> (url -> Text) -> Element
entryTemplate FeedEntry {..} render = Element "entry" Map.empty $ map NodeElement
    [ Element "id" Map.empty [NodeContent $ render feedEntryLink]
    , Element "link" (Map.singleton "href" $ render feedEntryLink) []
    , Element "updated" Map.empty [NodeContent $ formatW3 feedEntryUpdated]
    , Element "title" Map.empty [NodeContent feedEntryTitle]
    , Element "content" (Map.singleton "type" "html") [NodeContent $ toStrict $ renderHtml feedEntryContent]
    ]

-- | Generates a link tag in the head of a widget.
atomLink :: Route m
         -> Text -- ^ title
         -> GWidget s m ()
atomLink r title = toWidgetHead [hamlet|
$newline never
<link href=@{r} type=#{S8.unpack typeAtom} rel="alternate" title=#{title}>
|]
