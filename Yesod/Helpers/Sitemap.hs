{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import qualified Hack
import Yesod.Request
import Data.Time (UTCTime)
import Data.Convertible.Text (cs)

data SitemapLoc = AbsLoc String | RelLoc String
data SitemapChangeFreq = Always
                       | Hourly
                       | Daily
                       | Weekly
                       | Monthly
                       | Yearly
                       | Never
instance Show SitemapChangeFreq where
    show Always = "always"
    show Hourly = "hourly"
    show Daily = "daily"
    show Weekly = "weekly"
    show Monthly = "monthly"
    show Yearly = "yearly"
    show Never = "never"

data SitemapUrl = SitemapUrl
    { sitemapLoc :: SitemapLoc
    , sitemapLastMod :: UTCTime
    , sitemapChangeFreq :: SitemapChangeFreq
    , priority :: Double
    }
data SitemapRequest = SitemapRequest String Int
data SitemapResponse = SitemapResponse SitemapRequest [SitemapUrl]
instance Show SitemapResponse where -- FIXME very ugly, use Text instead
    show (SitemapResponse (SitemapRequest host port) urls) =
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++
        "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">" ++
        concatMap helper urls ++
        "</urlset>"
        where
            prefix = "http://" ++ host ++
                        case port of
                            80 -> ""
                            _ -> ':' : show port
            helper (SitemapUrl loc modTime freq pri) = concat
                [ "<url><loc>"
                , encodeHtml $ showLoc loc
                , "</loc><lastmod>"
                , formatW3 modTime
                , "</lastmod><changefreq>"
                , show freq
                , "</changefreq><priority>"
                , show pri
                , "</priority></url>"
                ]
            showLoc (AbsLoc s) = s
            showLoc (RelLoc s) = prefix ++ s

instance HasReps SitemapResponse where
    reps =
        [ (TypeXml, cs . show)
        ]

sitemap :: IO [SitemapUrl] -> Handler SitemapResponse
sitemap urls' = do
    env <- parseEnv
    -- FIXME
    let req = SitemapRequest (Hack.serverName env) (Hack.serverPort env)
    urls <- liftIO urls'
    return $ SitemapResponse req urls

robots :: Approot -> Handler Plain
robots (Approot ar) = do
    return $ plain $ "Sitemap: " ++ ar ++ "sitemap.xml"
