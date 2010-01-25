{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
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
    , SitemapResponse (..)
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
instance ConvertSuccess SitemapChangeFreq String where
    convertSuccess Always  = "always"
    convertSuccess Hourly  = "hourly"
    convertSuccess Daily   = "daily"
    convertSuccess Weekly  = "weekly"
    convertSuccess Monthly = "monthly"
    convertSuccess Yearly  = "yearly"
    convertSuccess Never   = "never"
instance ConvertSuccess SitemapChangeFreq Html where
    convertSuccess = (cs :: String -> Html) . cs

data SitemapUrl = SitemapUrl
    { sitemapLoc :: Location
    , sitemapLastMod :: UTCTime
    , sitemapChangeFreq :: SitemapChangeFreq
    , priority :: Double
    }
data SitemapResponse = SitemapResponse [SitemapUrl] Approot
instance ConvertSuccess SitemapResponse Content where
    convertSuccess = cs . (cs :: Html -> XmlDoc) . cs
instance ConvertSuccess SitemapResponse Html where
    convertSuccess (SitemapResponse urls ar) =
        Tag "urlset" [("xmlns", sitemapNS)] $ HtmlList $ map helper urls
          where
            sitemapNS = "http://www.sitemaps.org/schemas/sitemap/0.9"
            helper :: SitemapUrl -> Html
            helper (SitemapUrl loc modTime freq pri) =
                Tag "url" [] $ HtmlList
                    [ Tag "loc" [] $ cs $ showLocation ar loc
                    , Tag "lastmod" [] $ cs $ formatW3 modTime
                    , Tag "changefreq" [] $ cs freq
                    , Tag "priority" [] $ cs $ show pri
                    ]

instance HasReps SitemapResponse where
    chooseRep = defChooseRep
        [ (TypeXml, return . cs)
        ]

sitemap :: YesodApproot y => [SitemapUrl] -> Handler y SitemapResponse
sitemap urls = do
    yesod <- getYesod
    return $ SitemapResponse urls $ approot yesod

robots :: YesodApproot yesod => Handler yesod [(ContentType, Content)]
robots = do
    yesod <- getYesod
    return $ staticRep TypePlain $ "Sitemap: " ++ showLocation
                                      (approot yesod)
                                      (RelLoc "sitemap.xml")
