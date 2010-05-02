{-# LANGUAGE QuasiQuotes #-}
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

module Yesod.Helpers.Sitemap
    ( sitemap
    , robots
    , SitemapUrl (..)
    , SitemapChangeFreq (..)
    ) where

import Yesod
import Web.Encodings (formatW3)
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

sitemapNS :: [SitemapUrl url] -> HtmlContent
sitemapNS _ = cs "http://www.sitemaps.org/schemas/sitemap/0.9"

template :: [SitemapUrl url] -> Hamlet url IO ()
template = [$hamlet|
%urlset!xmlns=$sitemapNS$
    $forall id url
        %url
            %loc @url.sitemapLoc@
            %lastmod $url.sitemapLastMod.formatW3.cs$
            %changefreq $url.sitemapChangeFreq.showFreq.cs$
            %priority $url.priority.show.cs$
|]

sitemap :: [SitemapUrl (Routes master)] -> GHandler sub master RepXml
sitemap = fmap RepXml . hamletToContent . template

robots :: Routes sub -- ^ sitemap url
       -> GHandler sub master RepPlain
robots smurl = do
    r <- getUrlRender
    return $ RepPlain $ cs $ "Sitemap: " ++ r smurl
