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

import Yesod.Core
import Data.Time (UTCTime)
import Text.XML.Stream.Render (renderBuilder)
import Data.Text (Text, pack)
import Data.XML.Types
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Default (def)
import qualified Data.Text as T

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
    , sitemapLastMod :: Maybe UTCTime
    , sitemapChangeFreq :: Maybe SitemapChangeFreq
    , sitemapPriority :: Maybe Double
    }

-- | A basic robots file which just lists the "Sitemap: " line.
robots :: MonadHandler m
       => Route (HandlerSite m) -- ^ sitemap url
       -> m Text
robots smurl = do
    ur <- getUrlRender
    return $ T.unlines
        [ "Sitemap: " `T.append` ur smurl
        , "User-agent: *"
        ]

sitemap :: Source (HandlerT site IO) (SitemapUrl (Route site))
        -> HandlerT site IO TypedContent
sitemap urls = do
    render <- getUrlRender
    respondSource typeXml $ src render $= renderBuilder def $= CL.map Chunk
  where
    namespace = "http://www.sitemaps.org/schemas/sitemap/0.9"
    element name' attrs inside = do
        yield $ EventBeginElement name attrs
        () <- inside
        yield $ EventEndElement name
      where
        name = Name name' (Just namespace) Nothing

    src render = do
        yield EventBeginDocument
        element "urlset" [] $ do
            urls $= awaitForever goUrl
        yield EventEndDocument
      where
        goUrl SitemapUrl {..} = element "url" [] $ do
            element "loc" [] $ yield $ EventContent $ ContentText $ render sitemapLoc
            case sitemapLastMod of
                Nothing -> return ()
                Just lm -> element "lastmod" [] $ yield $ EventContent $ ContentText $ formatW3 lm
            case sitemapChangeFreq of
                Nothing -> return ()
                Just scf -> element "changefreq" [] $ yield $ EventContent $ ContentText $ showFreq scf
            element "priority" [] $ yield $ EventContent $ ContentText $ pack $ show sitemapPriority
