{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
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
      -- * FIXME
    , GenResponse (..)
    , ResponseWrapper (..)
    , ErrorResponse (..)
    , formatW3
    , UTCTime
    ) where

import Data.ByteString.Class
import qualified Hack
import Data.Time.Format
import Data.Time.Clock
import Web.Encodings
import System.Locale
import Data.Object
import Data.List (intercalate)

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

instance Response [(String, Hack.Response)] where
    reps = id

-- FIXME put in a separate module (maybe Web.Encodings)
formatW3 :: UTCTime -> String
formatW3 = formatTime defaultTimeLocale "%FT%X-08:00"
