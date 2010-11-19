{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}
---------------------------------------------------------
--
-- Module        : Yesod.Helpers.Sitemap
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
module Yesod.Helpers.Sitemap
    ( sitemap
    , robots
    , SitemapUrl (..)
    , SitemapChangeFreq (..)
    ) where

import Yesod
import Data.Time (UTCTime)

data SitemapChangeFreq = Always
                       | Hourly
                       | Daily
                       | Weekly
                       | Monthly
                       | Yearly
                       | Never

showFreq :: SitemapChangeFreq -> String
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
    , priority :: Double
    }

template :: [SitemapUrl url] -> Hamlet url
template urls =
#if GHC7
                [xhamlet|
#else
                [$xhamlet|
#endif
%urlset!xmlns="http://www.sitemaps.org/schemas/sitemap/0.9"
    $forall urls url
        %url
            %loc @sitemapLoc.url@
            %lastmod $formatW3.sitemapLastMod.url$
            %changefreq $showFreq.sitemapChangeFreq.url$
            %priority $show.priority.url$
|]

sitemap :: [SitemapUrl (Route master)] -> GHandler sub master RepXml
sitemap = fmap RepXml . hamletToContent . template

-- | A basic robots file which just lists the "Sitemap: " line.
robots :: Route sub -- ^ sitemap url
       -> GHandler sub master RepPlain
robots smurl = do
    tm <- getRouteToMaster
    render <- getUrlRender
    return $ RepPlain $ toContent $ "Sitemap: " ++ render (tm smurl)
