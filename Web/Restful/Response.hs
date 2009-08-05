{-# LANGUAGE ExistentialQuantification #-}
---------------------------------------------------------
--
-- Module        : Web.Restful.Response
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
-- Generating responses.
--
---------------------------------------------------------
module Web.Restful.Response
    (
      -- * Response construction
      Response (..)
    , response
      -- ** Helper 'Response' instances
      -- *** Atom news feed
    , AtomFeed (..)
    , AtomFeedEntry (..)
      -- *** Sitemap
    , sitemap
    , SitemapUrl (..)
    , SitemapLoc (..)
    , SitemapChangeFreq (..)
      -- *** Generics
      -- **** List/detail
    , ListDetail (..)
    , ItemList (..)
    , ItemDetail (..)
      -- **** Multiple response types.
    , GenResponse (..)
      -- * FIXME
    , ResponseWrapper (..)
    , ErrorResponse (..)
    ) where

import Data.ByteString.Class
import qualified Hack
import Data.Time.Format
import Data.Time.Clock
import Web.Encodings
import System.Locale
import Web.Restful.Request -- FIXME ultimately remove
import Data.Object
import Data.List (intercalate)
import Data.Object.Instances

type ContentType = String

-- | The output for a resource.
class Response a where
    -- | Provide an ordered list of possible responses, depending on content
    -- type. If the user asked for a specific response type (like
    -- text/html), then that will get priority. If not, then the first
    -- element in this list will be used.
    reps :: a -> [(ContentType, Hack.Response)]

-- | Wrapper around 'Hack.Response' to allow arbitrary pieces of data to be
-- used for the body.
response :: LazyByteString lbs
         => Int
         -> [(String, String)]
         -> lbs
         -> Hack.Response
response a b c = Hack.Response a b $ toLazyByteString c

instance Response () where
    reps _ = [("text/plain", response 200 [] "")]

newtype ErrorResponse = ErrorResponse String
instance Response ErrorResponse where
    reps (ErrorResponse s) = [("text/plain", response 500 [] s)]

data ResponseWrapper = forall res. Response res => ResponseWrapper res
instance Response ResponseWrapper where
    reps (ResponseWrapper res) = reps res

data AtomFeed = AtomFeed
    { atomTitle :: String
    , atomLinkSelf :: String
    , atomLinkHome :: String
    , atomUpdated :: UTCTime
    , atomEntries :: [AtomFeedEntry]
    }
instance Response AtomFeed where
    reps e =
        [ ("application/atom+xml", response 200 [] $ show e)
        ]

data AtomFeedEntry = AtomFeedEntry
    { atomEntryLink :: String
    , atomEntryUpdated :: UTCTime
    , atomEntryTitle :: String
    , atomEntryContent :: String
    }

instance Show AtomFeed where
    show f = concat
        [ "<?xml version='1.0' encoding='utf-8' ?>\n"
        , "<feed xmlns='http://www.w3.org/2005/Atom'>"
        , "<title>"
        , encodeHtml $ atomTitle f
        , "</title>"
        , "<link rel='self' href='"
        , encodeHtml $ atomLinkSelf f
        , "'/>"
        , "<link href='"
        , encodeHtml $ atomLinkHome f
        , "'/>"
        , "<updated>"
        , formatW3 $ atomUpdated f
        , "</updated>"
        , "<id>"
        , encodeHtml $ atomLinkHome f
        , "</id>"
        , concatMap show $ atomEntries f
        , "</feed>"
        ]

instance Show AtomFeedEntry where
    show e = concat
        [ "<entry>"
        , "<id>"
        , encodeHtml $ atomEntryLink e
        , "</id>"
        , "<link href='"
        , encodeHtml $ atomEntryLink e
        , "' />"
        , "<updated>"
        , formatW3 $ atomEntryUpdated e
        , "</updated>"
        , "<title>"
        , encodeHtml $ atomEntryTitle e
        , "</title>"
        , "<content type='html'><![CDATA["
        , atomEntryContent e
        , "]]></content>"
        , "</entry>"
        ]

formatW3 :: UTCTime -> String
formatW3 = formatTime defaultTimeLocale "%FT%X-08:00"

-- sitemaps
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

instance Response SitemapResponse where
    reps res =
        [ ("text/xml", response 200 [] $ show res)
        ]

sitemap :: IO [SitemapUrl] -> SitemapRequest -> IO SitemapResponse
sitemap urls' req = do
    urls <- urls'
    return $ SitemapResponse req urls

data GenResponse = HtmlResponse String
                 | ObjectResponse Object
                 | HtmlOrObjectResponse String Object
                 | RedirectResponse String
                 | PermissionDeniedResult String
                 | NotFoundResponse String
instance Response GenResponse where
    reps (HtmlResponse h) = [("text/html", response 200 [] h)]
    reps (ObjectResponse t) = reps t
    reps (HtmlOrObjectResponse h t) =
        ("text/html", response 200 [] h) : reps t
    reps (RedirectResponse url) = [("text/html", response 303 heads body)]
      where
        heads = [("Location", url)]
        body = "<p>Redirecting to <a href='" ++ encodeHtml url ++
               "'>" ++ encodeHtml url ++ "</a></p>"
    reps (PermissionDeniedResult s) = [("text/plain", response 403 [] s)]
    reps (NotFoundResponse s) = [("text/plain", response 404 [] s)]
class ToObject a => ListDetail a where
    htmlDetail :: a -> String
    htmlDetail = treeToHtml . toObject
    detailTitle :: a -> String
    detailUrl :: a -> String
    htmlList :: [a] -> String
    htmlList l = "<ul>" ++ concatMap helper l ++ "</ul>"
        where
            helper i = "<li><a href=\"" ++ encodeHtml (detailUrl i) ++
                       "\">" ++ encodeHtml (detailTitle i) ++
                       "</a></li>"
    -- | Often times for the JSON response of the list, we don't need all
    -- the information.
    treeList :: [a] -> Object -- FIXME
    treeList = Sequence . map treeListSingle
    treeListSingle :: a -> Object
    treeListSingle = toObject

newtype ItemList a = ItemList [a]
instance ListDetail a => Response (ItemList a) where
    reps (ItemList l) =
        [ ("text/html", response 200 [] $ htmlList l)
        , ("application/json", response 200 [] $ treeToJson $ treeList l)
        ]
newtype ItemDetail a = ItemDetail a
instance ListDetail a => Response (ItemDetail a) where
    reps (ItemDetail i) =
        [ ("text/html", response 200 [] $ htmlDetail i)
        , ("application/json", response 200 [] $ treeToJson $ toObject i)
        ]

-- FIXME remove treeTo functions, replace with Object instances
treeToJson :: Object -> String
treeToJson (Scalar s) = '"' : encodeJson (fromStrictByteString s) ++ "\""
treeToJson (Sequence l) =
    "[" ++ intercalate "," (map treeToJson l) ++ "]"
treeToJson (Mapping m) =
    "{" ++ intercalate "," (map helper m) ++ "}" where
        helper (k, v) =
            treeToJson (Scalar k) ++
            ":" ++
            treeToJson v

treeToHtml :: Object -> String
treeToHtml (Scalar s) = encodeHtml $ fromStrictByteString s
treeToHtml (Sequence l) =
    "<ul>" ++ concatMap (\e -> "<li>" ++ treeToHtml e ++ "</li>") l ++
    "</ul>"
treeToHtml (Mapping m) =
    "<dl>" ++
    concatMap (\(k, v) -> "<dt>" ++
                          encodeHtml (fromStrictByteString k) ++
                          "</dt>" ++
                          "<dd>" ++
                          treeToHtml v ++
                          "</dd>") m ++
    "</dl>"

instance Response Object where
    reps tree =
        [ ("text/html", response 200 [] $ treeToHtml tree)
        , ("application/json", response 200 [] $ treeToJson tree)
        ]
