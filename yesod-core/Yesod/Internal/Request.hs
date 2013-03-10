{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Yesod.Internal.Request
    ( parseWaiRequest
    , RequestBodyContents
    , FileInfo
    , fileName
    , fileContentType
    , fileSource
    , fileMove
    , mkFileInfoLBS
    , mkFileInfoFile
    , mkFileInfoSource
    , FileUpload (..)
    , tooLargeResponse
    -- The below are exported for testing.
    , randomString
    ) where

import Control.Arrow (second)
import qualified Network.Wai.Parse as NWP
import Yesod.Internal
import qualified Network.Wai as W
import System.Random (RandomGen, randomRs)
import Web.Cookie (parseCookiesText)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import Data.Text (Text, pack)
import Network.HTTP.Types (queryToQueryText, Status (Status))
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.ByteString.Lazy as L
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Conduit
import Data.Conduit.List (sourceList)
import Data.Conduit.Binary (sourceFile, sinkFile)
import Data.Word (Word64)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (throwIO)
import Yesod.Core.Types
import qualified Data.Map as Map

-- | Impose a limit on the size of the request body.
limitRequestBody :: Word64 -> W.Request -> W.Request
limitRequestBody maxLen req =
    req { W.requestBody = W.requestBody req $= limit maxLen }
  where
    tooLarge = liftIO $ throwIO $ HCWai tooLargeResponse

    limit 0 = tooLarge
    limit remaining =
        await >>= maybe (return ()) go
      where
        go bs = do
            let len = fromIntegral $ S8.length bs
            if len > remaining
                then tooLarge
                else do
                    yield bs
                    limit $ remaining - len

tooLargeResponse :: W.Response
tooLargeResponse = W.responseLBS
    (Status 413 "Too Large")
    [("Content-Type", "text/plain")]
    "Request body too large to be processed."

parseWaiRequest :: RandomGen g
                => W.Request
                -> SessionMap
                -> (ErrorResponse -> YesodApp)
                -> Bool
                -> Word64 -- ^ max body size
                -> g
                -> YesodRequest
parseWaiRequest env session onError useToken maxBodySize gen =
    YesodRequest
        { reqGetParams  = gets
        , reqCookies    = cookies
        , reqWaiRequest = limitRequestBody maxBodySize env
        , reqLangs      = langs''
        , reqToken      = token
        , reqSession    = session
        , reqAccept     = httpAccept env
        , reqOnError    = onError
        }
  where
    gets = map (second $ fromMaybe "")
         $ queryToQueryText
         $ W.queryString env
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
    token = if not useToken
              then Nothing
              else Just $ maybe
                            (pack $ randomString 10 gen)
                            (decodeUtf8With lenientDecode)
                            (Map.lookup tokenKey session)

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
    filter (flip Set.notMember exist) $ toAdd []
addTwoLetters (toAdd, exist) (l:ls) =
    l : addTwoLetters (toAdd', exist') ls
  where
    (toAdd', exist')
        | T.length l > 2 = (toAdd . (T.take 2 l:), exist)
        | otherwise = (toAdd, Set.insert l exist)

-- | Generate a random String of alphanumerical characters
-- (a-z, A-Z, and 0-9) of the given length using the given
-- random number generator.
randomString :: RandomGen g => Int -> g -> String
randomString len = take len . map toChar . randomRs (0, 61)
  where
    toChar i
        | i < 26 = toEnum $ i + fromEnum 'A'
        | i < 52 = toEnum $ i + fromEnum 'a' - 26
        | otherwise = toEnum $ i + fromEnum '0' - 52

mkFileInfoLBS :: Text -> Text -> L.ByteString -> FileInfo
mkFileInfoLBS name ct lbs = FileInfo name ct (sourceList $ L.toChunks lbs) (\fp -> L.writeFile fp lbs)

mkFileInfoFile :: Text -> Text -> FilePath -> FileInfo
mkFileInfoFile name ct fp = FileInfo name ct (sourceFile fp) (\dst -> runResourceT $ sourceFile fp $$ sinkFile dst)

mkFileInfoSource :: Text -> Text -> Source (ResourceT IO) ByteString -> FileInfo
mkFileInfoSource name ct src = FileInfo name ct src (\dst -> runResourceT $ src $$ sinkFile dst)
