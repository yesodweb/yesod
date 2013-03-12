{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Yesod.Core.Internal.Response where

import           Blaze.ByteString.Builder     (toByteString)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as S
import qualified Data.ByteString.Char8        as S8
import           Data.CaseInsensitive         (CI)
import qualified Data.CaseInsensitive         as CI
import           Network.Wai
import           Prelude                      hiding (catch)
import           Web.Cookie                   (renderSetCookie)
import           Yesod.Core.Content
import           Yesod.Core.Types

yarToResponse :: YesodResponse -> [(CI ByteString, ByteString)] -> Response
yarToResponse (YRWai a) _ = a
yarToResponse (YRPlain s hs _ c _) extraHeaders =
    go c
  where
    finalHeaders = extraHeaders ++ map headerToPair hs
    finalHeaders' len = ("Content-Length", S8.pack $ show len)
                      : finalHeaders

    go (ContentBuilder b mlen) =
        ResponseBuilder s hs' b
      where
        hs' = maybe finalHeaders finalHeaders' mlen
    go (ContentFile fp p) = ResponseFile s finalHeaders fp p
    go (ContentSource body) = ResponseSource s finalHeaders body
    go (ContentDontEvaluate c') = go c'

-- | Convert Header to a key/value pair.
headerToPair :: Header
             -> (CI ByteString, ByteString)
headerToPair (AddCookie sc) =
    ("Set-Cookie", toByteString $ renderSetCookie $ sc)
headerToPair (DeleteCookie key path) =
    ( "Set-Cookie"
    , S.concat
        [ key
        , "=; path="
        , path
        , "; expires=Thu, 01-Jan-1970 00:00:00 GMT"
        ]
    )
headerToPair (Header key value) = (CI.mk key, value)
