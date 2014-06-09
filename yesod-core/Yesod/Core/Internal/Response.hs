{-# LANGUAGE CPP               #-}
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
#if MIN_VERSION_wai(2, 0, 0)
import           Data.Conduit                 (transPipe)
import           Control.Monad.Trans.Resource (runInternalState, getInternalState, runResourceT, InternalState, closeInternalState)
import           Control.Monad.Trans.Class    (lift)
import           Network.Wai.Internal
import           Control.Exception            (finally)
#endif
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
import           Data.Conduit                 (Flush (..), ($$))
import qualified Data.Conduit.List            as CL

#if MIN_VERSION_wai(3, 0, 0)
yarToResponse :: YesodResponse
              -> (SessionMap -> IO [Header]) -- ^ save session
              -> YesodRequest
              -> Request
              -> InternalState
              -> IO Response
yarToResponse (YRWai a) _ _ _ _ = return a
yarToResponse (YRPlain s' hs ct c newSess) saveSession yreq req is = do
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

    let go (ContentBuilder b mlen) = do
            let hs' = maybe finalHeaders finalHeaders' mlen
            return $ ResponseBuilder s hs' b
        go (ContentFile fp p) = do
            return $ ResponseFile s finalHeaders fp p
        go (ContentSource body) = return $ responseStream s finalHeaders
            $ \sendChunk flush -> do
                transPipe (flip runInternalState is) body
                $$ CL.mapM_ (\mchunk ->
                    case mchunk of
                        Flush -> flush
                        Chunk builder -> sendChunk builder)
        go (ContentDontEvaluate c') = go c'
    go c
  where
    s
        | s' == defaultStatus = H.status200
        | otherwise = s'

#else
yarToResponse :: YesodResponse
              -> (SessionMap -> IO [Header]) -- ^ save session
              -> YesodRequest
              -> Request
#if MIN_VERSION_wai(2, 0, 0)
              -> InternalState
#endif
              -> IO Response
#if MIN_VERSION_wai(2, 0, 0)
yarToResponse (YRWai a) _ _ _ is =
    case a of
        ResponseSource s hs w -> return $ ResponseSource s hs $ \f ->
            w f `finally` closeInternalState is
        ResponseBuilder{} -> do
            closeInternalState is
            return a
        ResponseFile{} -> do
            closeInternalState is
            return a
#if MIN_VERSION_wai(2, 1, 0)
        -- Ignore the fallback provided, in case it refers to a ResourceT state
        -- in a ResponseSource.
        ResponseRaw raw _ -> return $ ResponseRaw
            (\f -> raw f `finally` closeInternalState is)
            (responseLBS H.status500 [("Content-Type", "text/plain")]
                "yarToResponse: backend does not support raw responses")
#endif
#else
yarToResponse (YRWai a) _ _ _ = return a
#endif
yarToResponse (YRPlain s' hs ct c newSess) saveSession yreq req
#if MIN_VERSION_wai(2, 0, 0)
  is
#endif
  = do
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

#if MIN_VERSION_wai(2, 0, 0)
    let go (ContentBuilder b mlen) = do
            let hs' = maybe finalHeaders finalHeaders' mlen
            closeInternalState is
            return $ ResponseBuilder s hs' b
        go (ContentFile fp p) = do
            closeInternalState is
            return $ ResponseFile s finalHeaders fp p
        go (ContentSource body) = return $ ResponseSource s finalHeaders $ \f ->
            f (transPipe (flip runInternalState is) body) `finally`
            closeInternalState is
        go (ContentDontEvaluate c') = go c'
    go c
#else
    let go (ContentBuilder b mlen) =
            let hs' = maybe finalHeaders finalHeaders' mlen
             in ResponseBuilder s hs' b
        go (ContentFile fp p) = ResponseFile s finalHeaders fp p
        go (ContentSource body) = ResponseSource s finalHeaders body
        go (ContentDontEvaluate c') = go c'
    return $ go c
#endif
  where
    s
        | s' == defaultStatus = H.status200
        | otherwise = s'
#endif

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
        len = L.length lbs
        mlen' = maybe (Just $ fromIntegral len) Just mlen
    len `seq` return (Right $ ContentBuilder (fromLazyByteString lbs) mlen')
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
