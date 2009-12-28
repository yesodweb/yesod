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

import Yesod.Definitions
import Yesod.Handler
import Yesod.Rep
import Web.Encodings
import Data.Time (UTCTime)
import Data.Convertible.Text
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import Yesod.Yesod

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

data SitemapUrl = SitemapUrl
    { sitemapLoc :: Location
    , sitemapLastMod :: UTCTime
    , sitemapChangeFreq :: SitemapChangeFreq
    , priority :: Double
    }
data SitemapResponse = SitemapResponse [SitemapUrl] Approot
instance ConvertSuccess SitemapResponse Content where
    convertSuccess = cs . (cs :: SitemapResponse -> Text)
instance ConvertSuccess SitemapResponse Text where
    convertSuccess (SitemapResponse urls ar) = TL.concat
        [ cs "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
        , cs "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">"
        , TL.concat $ map helper urls
        , cs "</urlset>"
        ]
        where
            helper (SitemapUrl loc modTime freq pri) = cs $ concat
                -- FIXME use HTML?
                [ "<url><loc>"
                , encodeHtml $ showLocation ar loc
                , "</loc><lastmod>"
                , formatW3 modTime
                , "</lastmod><changefreq>"
                , cs freq
                , "</changefreq><priority>"
                , show pri
                , "</priority></url>"
                ]

instance HasReps SitemapResponse where
    reps =
        [ (TypeXml, return . cs)
        ]

sitemap :: YesodApproot y => [SitemapUrl] -> Handler y SitemapResponse
sitemap urls = do
    yesod <- getYesod
    return $ SitemapResponse urls $ approot yesod

robots :: YesodApproot yesod => Handler yesod Plain
robots = do
    yesod <- getYesod
    return $ plain $ "Sitemap: " ++ showLocation
                                      (approot yesod)
                                      (RelLoc "sitemap.xml")
