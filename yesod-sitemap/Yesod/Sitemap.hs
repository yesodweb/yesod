{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
---------------------------------------------------------
--
-- Module        : Yesod.Sitemap
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
-- Generating Google sitemap files.
--
---------------------------------------------------------

-- | Generates XML sitemap files.
--
-- See <http://www.sitemaps.org/>.
module Yesod.Sitemap
    ( sitemap
    , robots
    , SitemapUrl (..)
    , SitemapChangeFreq (..)
    ) where

import Yesod.Content (RepXml (..), RepPlain (..), toContent, formatW3)
import Yesod.Core (Route, GHandler, getUrlRender)
import Data.Time (UTCTime)
import Data.Monoid (mappend)
import Text.XML
import Data.Text (Text, pack)

data SitemapChangeFreq = Always
                       | Hourly
                       | Daily
                       | Weekly
                       | Monthly
                       | Yearly
                       | Never

showFreq :: SitemapChangeFreq -> Text
showFreq Always  = "always"
showFreq Hourly  = "hourly"
showFreq Daily   = "daily"
showFreq Weekly  = "weekly"
showFreq Monthly = "monthly"
showFreq Yearly  = "yearly"
showFreq Never   = "never"

data SitemapUrl url = SitemapUrl
    { sitemapLoc :: url
    , sitemapLastMod :: UTCTime
    , sitemapChangeFreq :: SitemapChangeFreq
    , sitemapPriority :: Double
    }

template :: [SitemapUrl url]
         -> (url -> Text)
         -> Document
template urls render =
    Document (Prologue [] Nothing []) (addNS root) []
  where
    addNS (Element (Name ln _ _) as ns) = Element (Name ln (Just namespace) Nothing) as (map addNS' ns)
    addNS' (NodeElement e) = NodeElement (addNS e)
    addNS' n = n
    namespace = "http://www.sitemaps.org/schemas/sitemap/0.9"

    root = Element "urlset" [] $ map go urls

    go SitemapUrl {..} = NodeElement $ Element "url" [] $ map NodeElement
        [ Element "loc" [] [NodeContent $ render sitemapLoc]
        , Element "lastmod" [] [NodeContent $ formatW3 sitemapLastMod]
        , Element "changefreq" [] [NodeContent $ showFreq sitemapChangeFreq]
        , Element "priority" [] [NodeContent $ pack $ show sitemapPriority]
        ]

sitemap :: [SitemapUrl (Route master)] -> GHandler sub master RepXml
sitemap urls = do
    render <- getUrlRender
    let doc = template urls render
    return $ RepXml $ toContent $ renderLBS def doc

-- | A basic robots file which just lists the "Sitemap: " line.
robots :: Route master -- ^ sitemap url
       -> GHandler sub master RepPlain
robots smurl = do
    render <- getUrlRender
    return $ RepPlain $ toContent $ "Sitemap: " `mappend` render smurl
