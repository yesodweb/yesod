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
import qualified Network.HTTP.Types           as H
import qualified Data.Text                    as T
import           Control.Exception            (SomeException, handle)
import           Blaze.ByteString.Builder     (fromLazyByteString,
                                               toLazyByteString)
import qualified Data.ByteString.Lazy         as L

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

evaluateContent :: Content -> IO (Either ErrorResponse Content)
evaluateContent (ContentBuilder b mlen) = handle f $ do
    let lbs = toLazyByteString b
    L.length lbs `seq` return (Right $ ContentBuilder (fromLazyByteString lbs) mlen)
  where
    f :: SomeException -> IO (Either ErrorResponse Content)
    f = return . Left . InternalError . T.pack . show
evaluateContent c = return (Right c)

getStatus :: ErrorResponse -> H.Status
getStatus NotFound = H.status404
getStatus (InternalError _) = H.status500
getStatus (InvalidArgs _) = H.status400
getStatus (PermissionDenied _) = H.status403
getStatus (BadMethod _) = H.status405
