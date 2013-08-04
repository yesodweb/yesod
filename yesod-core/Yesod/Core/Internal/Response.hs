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
import qualified Data.Map                     as Map
import           Yesod.Core.Internal.Request  (tokenKey)
import           Data.Text.Encoding           (encodeUtf8)

yarToResponse :: Monad m
              => YesodResponse
              -> (SessionMap -> m [Header]) -- ^ save session
              -> YesodRequest
              -> m Response
yarToResponse (YRWai a) _ _ = return a
yarToResponse (YRPlain s' hs ct c newSess) saveSession yreq = do
    extraHeaders <- do
        let nsToken = maybe
                newSess
                (\n -> Map.insert tokenKey (encodeUtf8 n) newSess)
                (reqToken yreq)
        sessionHeaders <- saveSession nsToken
        return $ ("Content-Type", ct) : map headerToPair sessionHeaders
    let finalHeaders = extraHeaders ++ map headerToPair hs
        finalHeaders' len = ("Content-Length", S8.pack $ show len)
                          : finalHeaders
    let go (ContentBuilder b mlen) =
            let hs' = maybe finalHeaders finalHeaders' mlen
             in ResponseBuilder s hs' b
        go (ContentFile fp p) = ResponseFile s finalHeaders fp p
        go (ContentSource body) = ResponseSource s finalHeaders body
        go (ContentDontEvaluate c') = go c'
    return $ go c
  where
    s
        | s' == defaultStatus = H.status200
        | otherwise = s'

-- | Indicates that the user provided no specific status code to be used, and
-- therefore the default status code should be used. For normal responses, this
-- would be a 200 response, whereas for error responses this would be an
-- appropriate status code.
--
-- For more information on motivation for this, see:
--
-- https://groups.google.com/d/msg/yesodweb/vHDBzyu28TM/bezCvviWp4sJ
--
-- Since 1.2.3.1
defaultStatus :: H.Status
defaultStatus = H.mkStatus (-1) "INVALID DEFAULT STATUS"

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
getStatus NotAuthenticated = H.status401
getStatus (PermissionDenied _) = H.status403
getStatus (BadMethod _) = H.status405
