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
    , sitemapList
    , sitemapConduit
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

-- | Serve a stream of @SitemapUrl@s as a sitemap.
--
-- Since 1.2.0
sitemap :: Source (HandlerT site IO) (SitemapUrl (Route site))
        -> HandlerT site IO TypedContent
sitemap urls = do
    render <- getUrlRender
    respondSource typeXml $ do
        yield Flush
        urls $= sitemapConduit render $= renderBuilder def $= CL.map Chunk

-- | Convenience wrapper for @sitemap@ for the case when the input is an
-- in-memory list.
--
-- Since 1.2.0
sitemapList :: [SitemapUrl (Route site)] -> HandlerT site IO TypedContent
sitemapList = sitemap . mapM_ yield

-- | Convert a stream of @SitemapUrl@s to XML @Event@s using the given URL
-- renderer.
--
-- This function is fully general for usage outside of Yesod.
--
-- Since 1.2.0
sitemapConduit :: Monad m
               => (a -> Text)
               -> Conduit (SitemapUrl a) m Event
sitemapConduit render = do
    yield EventBeginDocument
    element "urlset" [] $ awaitForever goUrl
    yield EventEndDocument
  where
    namespace = "http://www.sitemaps.org/schemas/sitemap/0.9"
    element name' attrs inside = do
        yield $ EventBeginElement name attrs
        () <- inside
        yield $ EventEndElement name
      where
        name = Name name' (Just namespace) Nothing

    goUrl SitemapUrl {..} = element "url" [] $ do
        element "loc" [] $ yield $ EventContent $ ContentText $ render sitemapLoc
        case sitemapLastMod of
            Nothing -> return ()
            Just lm -> element "lastmod" [] $ yield $ EventContent $ ContentText $ formatW3 lm
        case sitemapChangeFreq of
            Nothing -> return ()
            Just scf -> element "changefreq" [] $ yield $ EventContent $ ContentText $ showFreq scf
        case sitemapPriority of
            Nothing -> return ()
            Just p -> element "priority" [] $ yield $ EventContent $ ContentText $ pack $ show p
