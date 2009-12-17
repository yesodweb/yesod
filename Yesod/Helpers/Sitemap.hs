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
    , SitemapLoc (..)
    , SitemapChangeFreq (..)
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

data SitemapLoc = AbsLoc String | RelLoc String
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
    { sitemapLoc :: SitemapLoc
    , sitemapLastMod :: UTCTime
    , sitemapChangeFreq :: SitemapChangeFreq
    , priority :: Double
    }
data SitemapResponse = SitemapResponse [SitemapUrl] Approot
instance ConvertSuccess SitemapResponse Content where
    convertSuccess = cs . (cs :: SitemapResponse -> Text)
instance ConvertSuccess SitemapResponse Text where
    convertSuccess (SitemapResponse urls (Approot ar)) = TL.concat
        [ cs "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
        , cs "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">"
        , TL.concat $ map helper urls
        , cs "</urlset>"
        ]
        where
            helper (SitemapUrl loc modTime freq pri) = cs $ concat
                [ "<url><loc>"
                , encodeHtml $ showLoc loc
                , "</loc><lastmod>"
                , formatW3 modTime
                , "</lastmod><changefreq>"
                , cs freq
                , "</changefreq><priority>"
                , show pri
                , "</priority></url>"
                ]
            showLoc (AbsLoc s) = s
            showLoc (RelLoc s) = ar ++ s

instance HasReps SitemapResponse where
    reps =
        [ (TypeXml, return . cs)
        ]

sitemap :: YesodApproot yesod
        => IO [SitemapUrl]
        -> Handler yesod SitemapResponse
sitemap urls' = do
    yesod <- getYesod
    urls <- liftIO urls'
    return $ SitemapResponse urls $ approot yesod

robots :: YesodApproot yesod => Handler yesod Plain
robots = do
    yesod <- getYesod
    return $ plain $ "Sitemap: " ++ unApproot (approot yesod)
                                 ++ "sitemap.xml"
