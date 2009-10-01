---------------------------------------------------------
--
-- Module        : Web.Restful.Response.AtomFeed
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

module Web.Restful.Response.Sitemap
    ( sitemap
    , SitemapUrl (..)
    , SitemapLoc (..)
    , SitemapChangeFreq (..)
    ) where

import Web.Restful.Handler
import Web.Restful.Response
import Web.Encodings
import qualified Hack
import Web.Restful.Request
import Data.ByteString.Class
import Data.Time (UTCTime)

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
instance Request SitemapRequest where
    parseRequest = do
        env <- parseEnv
        return $! SitemapRequest (Hack.serverName env)
                                 (Hack.serverPort env)
data SitemapResponse = SitemapResponse SitemapRequest [SitemapUrl]
instance Show SitemapResponse where
    show (SitemapResponse (SitemapRequest host port) urls) =
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++
        "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">" ++
        concatMap helper urls ++
        "</urlset>"
        where
            prefix = "http://" ++ host ++
                        case port of
                            80 -> ""
                            _ -> ":" ++ show port
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
    reps res =
        [ ("text/xml", toLazyByteString $ show res)
        ]

sitemap :: IO [SitemapUrl] -> Handler
sitemap urls' = do
    req <- getRequest
    urls <- liftIO urls'
    return $ reps $ SitemapResponse req urls
