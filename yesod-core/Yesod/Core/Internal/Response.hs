{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Yesod.Core.Internal.Response where

import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as S
import qualified Data.ByteString.Char8        as S8
import qualified Data.ByteString.Lazy         as BL
import           Data.CaseInsensitive         (CI)
import qualified Data.CaseInsensitive         as CI
import           Network.Wai
import           Control.Monad                (mplus)
import           Control.Monad.Trans.Resource (runInternalState, InternalState)
import           Network.Wai.Internal
import           Web.Cookie                   (renderSetCookie)
import           Yesod.Core.Content
import           Yesod.Core.Types
import qualified Network.HTTP.Types           as H
import qualified Data.Text                    as T
import           Control.Exception            (SomeException, handle)
import           Data.ByteString.Builder      (lazyByteString, toLazyByteString)
import qualified Data.ByteString.Lazy         as L
import qualified Data.Map                     as Map
import           Yesod.Core.Internal.Request  (tokenKey)
import           Data.Text.Encoding           (encodeUtf8)
import           Conduit

yarToResponse :: YesodResponse
              -> (SessionMap -> IO [Header]) -- ^ save session
              -> YesodRequest
              -> Request
              -> InternalState
              -> (Response -> IO ResponseReceived)
              -> IO ResponseReceived
yarToResponse (YRWai a) _ _ _ _ sendResponse = sendResponse a
yarToResponse (YRWaiApp app) _ _ req _ sendResponse = app req sendResponse
yarToResponse (YRPlain s' hs ct c newSess) saveSession yreq _req is sendResponse = do
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
            sendResponse $ ResponseBuilder s hs' b
        go (ContentFile fp p) = sendResponse $ ResponseFile s finalHeaders fp p
        go (ContentSource body) = sendResponse $ responseStream s finalHeaders
            $ \sendChunk flush -> runConduit $
                transPipe (`runInternalState` is) body
                .| mapM_C (\mchunk ->
                    case mchunk of
                        Flush -> flush
                        Chunk builder -> sendChunk builder)
        go (ContentDontEvaluate c') = go c'
    go c
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
    ("Set-Cookie", BL.toStrict $ toLazyByteString $ renderSetCookie sc)
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
        mlen' = mlen `mplus` Just (fromIntegral len)
    len `seq` return (Right $ ContentBuilder (lazyByteString lbs) mlen')
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
