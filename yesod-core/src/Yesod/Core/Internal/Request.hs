{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Yesod.Core.Internal.Request
    ( parseWaiRequest
    , RequestBodyContents
    , FileInfo
    , fileName
    , fileContentType
    , fileMove
    , mkFileInfoLBS
    , mkFileInfoFile
    , mkFileInfoSource
    , FileUpload (..)
    , tooLargeResponse
    , tokenKey
    , langKey
    , textQueryString
    -- The below are exported for testing.
    , randomString
    ) where

import Data.String (IsString)
import Control.Arrow (second)
import qualified Network.Wai.Parse as NWP
import qualified Network.Wai as W
import Web.Cookie (parseCookiesText)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as LS8
import Data.Text (Text, pack)
import Network.HTTP.Types (queryToQueryText, Status (Status))
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.ByteString.Lazy as L
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With, decodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Conduit
import Data.Word (Word8, Word64)
import Control.Exception (throwIO)
import Control.Monad ((<=<), liftM)
import Yesod.Core.Types
import qualified Data.Map as Map
import Data.IORef
import qualified Data.Vector.Storable as V
import Data.ByteString.Internal (ByteString (PS))
import qualified Data.Word8 as Word8

-- | Impose a limit on the size of the request body.
limitRequestBody :: Word64 -> W.Request -> IO W.Request
limitRequestBody maxLen req = do
    ref <- newIORef maxLen
    let bd = do
#if MIN_VERSION_wai(3,2,2)
            bs <- W.getRequestBodyChunk req
#else
            bs <- W.requestBody req
#endif
            remaining <- readIORef ref
            let len = fromIntegral $ S8.length bs
                remaining' = remaining - len
            if remaining < len
                then throwIO $ HCWai $ tooLargeResponse maxLen len
                else do
                    writeIORef ref remaining'
                    return bs
#if MIN_VERSION_wai(3,2,4)
    return $ W.setRequestBodyChunks bd req
#else
    return req { W.requestBody = bd }
#endif

tooLargeResponse :: Word64 -> Word64 -> W.Response
tooLargeResponse maxLen bodyLen = W.responseLBS
    (Status 413 "Too Large")
    [("Content-Type", "text/plain")]
    (L.concat
        [ "Request body too large to be processed. The maximum size is "
        , (LS8.pack (show maxLen))
        , " bytes; your request body was "
        , (LS8.pack (show bodyLen))
        , " bytes. If you're the developer of this site, you can configure the maximum length with the `maximumContentLength` or `maximumContentLengthIO` function on the Yesod typeclass."
        ])

parseWaiRequest :: W.Request
                -> SessionMap
                -> Bool
                -> Maybe Word64 -- ^ max body size
                -> Either (IO YesodRequest) (IO Int -> IO YesodRequest)
parseWaiRequest env session useToken mmaxBodySize =
    -- In most cases, we won't need to generate any random values. Therefore,
    -- we split our results: if we need a random generator, return a Right
    -- value, otherwise return a Left and avoid the relatively costly generator
    -- acquisition.
    case etoken of
        Left token -> Left $ mkRequest token
        Right mkToken -> Right $ mkRequest <=< mkToken
  where
    mkRequest token' = do
        envLimited <- maybe return limitRequestBody mmaxBodySize env
        return YesodRequest
            { reqGetParams  = gets
            , reqCookies    = cookies
            , reqWaiRequest = envLimited
            , reqLangs      = langs''
            , reqToken      = token'
            , reqSession    = if useToken
                                then Map.delete tokenKey session
                                else session
            , reqAccept     = httpAccept env
            }
    gets = textQueryString env
    reqCookie = lookup "Cookie" $ W.requestHeaders env
    cookies = maybe [] parseCookiesText reqCookie
    acceptLang = lookup "Accept-Language" $ W.requestHeaders env
    langs = map (pack . S8.unpack) $ maybe [] NWP.parseHttpAccept acceptLang

    lookupText k = fmap (decodeUtf8With lenientDecode) . Map.lookup k

    -- The language preferences are prioritized as follows:
    langs' = catMaybes [ lookup langKey gets -- Query _LANG
                       , lookup langKey cookies     -- Cookie _LANG
                       , lookupText langKey session -- Session _LANG
                       ] ++ langs                    -- Accept-Language(s)

    -- Github issue #195. We want to add an extra two-letter version of any
    -- language in the list.
    langs'' = addTwoLetters (id, Set.empty) langs'

    -- If sessions are disabled tokens should not be used (any
    -- tokenKey present in the session is ignored). If sessions
    -- are enabled and a session has no tokenKey a new one is
    -- generated.
    etoken
        | useToken =
            case Map.lookup tokenKey session of
                -- Already have a token, use it.
                Just bs -> Left $ Just $ decodeUtf8With lenientDecode bs
                -- Don't have a token, get a random generator and make a new one.
                Nothing -> Right $ fmap Just . randomString 40
        | otherwise = Left Nothing

textQueryString :: W.Request -> [(Text, Text)]
textQueryString = map (second $ fromMaybe "") . queryToQueryText . W.queryString

-- | Get the list of accepted content types from the WAI Request\'s Accept
-- header.
--
-- Since 1.2.0
httpAccept :: W.Request -> [ContentType]
httpAccept = NWP.parseHttpAccept
           . fromMaybe S8.empty
           . lookup "Accept"
           . W.requestHeaders

addTwoLetters :: ([Text] -> [Text], Set.Set Text) -> [Text] -> [Text]
addTwoLetters (toAdd, exist) [] =
    filter (`Set.notMember` exist) $ toAdd []
addTwoLetters (toAdd, exist) (l:ls) =
    l : addTwoLetters (toAdd', exist') ls
  where
    (toAdd', exist')
        | T.length l > 2 = (toAdd . (T.take 2 l:), exist)
        | otherwise = (toAdd, Set.insert l exist)

-- | Generate a random String of alphanumerical characters
-- (a-z, A-Z, and 0-9) of the given length using the given
-- random number generator.
randomString :: Monad m => Int -> m Int -> m Text
randomString len gen =
    liftM (decodeUtf8 . fromByteVector) $ V.replicateM len asciiChar
  where
    asciiChar =
      let loop = do
            x <- gen
            let y = fromIntegral $ x `mod` 64
            case () of
              ()
                | y < 26 -> return $ y + Word8._A
                | y < 52 -> return $ y + Word8._a - 26
                | y < 62 -> return $ y + Word8._0 - 52
                | otherwise -> loop
       in loop

fromByteVector :: V.Vector Word8 -> ByteString
fromByteVector v =
    PS fptr offset idx
  where
    (fptr, offset, idx) = V.unsafeToForeignPtr v
{-# INLINE fromByteVector #-}

mkFileInfoLBS :: Text -> Text -> L.ByteString -> FileInfo
mkFileInfoLBS name ct lbs =
    FileInfo name ct (sourceLazy lbs) (`L.writeFile` lbs)

mkFileInfoFile :: Text -> Text -> FilePath -> FileInfo
mkFileInfoFile name ct fp = FileInfo name ct (sourceFile fp) (\dst -> runConduitRes $ sourceFile fp .| sinkFile dst)

mkFileInfoSource :: Text -> Text -> ConduitT () ByteString (ResourceT IO) () -> FileInfo
mkFileInfoSource name ct src = FileInfo name ct src (\dst -> runConduitRes $ src .| sinkFile dst)

tokenKey :: IsString a => a
tokenKey = "_TOKEN"

langKey :: IsString a => a
langKey = "_LANG"
