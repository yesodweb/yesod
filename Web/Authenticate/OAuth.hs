{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Web.Authenticate.OAuth
    ( -- * Data types
      OAuth(..), SignMethod(..), Credential(..), OAuthException(..),
      -- * Operations for credentials
      emptyCredential, insert, delete, inserts,
      -- * Signature
      signOAuth, genSign,
      -- * Url & operation for authentication
      authorizeUrl, getAccessToken, getTemporaryCredential,
      getTokenCredential,
      -- * Utility Methods
      paramEncode
    ) where
import Network.HTTP.Enumerator
import Web.Authenticate.Internal (qsUrl)
import Data.Data
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe
import Control.Applicative
import Network.HTTP.Types (parseSimpleQuery)
import Control.Exception
import Control.Monad
import Data.List (sortBy)
import System.Random
import Data.Char
import Data.Digest.Pure.SHA
import Data.ByteString.Base64
import Data.Time
import Numeric
import Codec.Crypto.RSA (rsassa_pkcs1_v1_5_sign, ha_SHA1, PrivateKey(..))
import Network.HTTP.Types (Header)
import Control.Arrow (second)
import Blaze.ByteString.Builder (toByteString)
import Data.Enumerator (($$), run_, Stream (..), continue)
import Data.Monoid (mconcat)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IORef (newIORef, readIORef, atomicModifyIORef)
import Control.Exception (Exception, throwIO)

-- | Data type for OAuth client (consumer).
data OAuth = OAuth { oauthServerName      :: String        -- ^ Service name
                   , oauthRequestUri      :: String        -- ^ URI to request temporary credential
                   , oauthAccessTokenUri  :: String        -- ^ Uri to obtain access token
                   , oauthAuthorizeUri    :: String        -- ^ Uri to authorize
                   , oauthSignatureMethod :: SignMethod    -- ^ Signature Method
                   , oauthConsumerKey     :: BS.ByteString -- ^ Consumer key
                   , oauthConsumerSecret  :: BS.ByteString -- ^ Consumer Secret
                   , oauthCallback        :: Maybe BS.ByteString -- ^ Callback uri to redirect after authentication
                   } deriving (Show, Eq, Ord, Read, Data, Typeable)


-- | Data type for signature method.
data SignMethod = PLAINTEXT
                | HMACSHA1
                | RSASHA1 PrivateKey
                  deriving (Show, Eq, Ord, Read, Data, Typeable)

deriving instance Typeable PrivateKey
deriving instance Data PrivateKey
deriving instance Read PrivateKey
deriving instance Ord PrivateKey
deriving instance Eq PrivateKey

-- | Data type for redential.
data Credential = Credential { unCredential :: [(BS.ByteString, BS.ByteString)] }
                  deriving (Show, Eq, Ord, Read, Data, Typeable)

-- | Empty credential.
emptyCredential :: Credential
emptyCredential = Credential []

token, tokenSecret :: Credential -> BS.ByteString
token = fromMaybe "" . lookup "oauth_token" . unCredential
tokenSecret = fromMaybe "" . lookup "oauth_token_secret" . unCredential

data OAuthException = OAuthException String
                      deriving (Show, Eq, Data, Typeable)

instance Exception OAuthException

toStrict :: BSL.ByteString -> BS.ByteString
toStrict = BS.concat . BSL.toChunks

fromStrict :: BS.ByteString -> BSL.ByteString
fromStrict = BSL.fromChunks . return

-- | Get temporary credential for requesting acces token.
getTemporaryCredential :: OAuth         -- ^ OAuth Application
                       -> IO Credential -- ^ Temporary Credential (Request Token & Secret).
getTemporaryCredential oa = do
  let req = fromJust $ parseUrl $ oauthRequestUri oa
  req' <- signOAuth oa emptyCredential (req { method = "POST" }) 
  rsp <- withManager $ httpLbs req'
  if statusCode rsp == 200
    then do
      let dic = parseSimpleQuery . toStrict . responseBody $ rsp
      return $ Credential dic
    else throwIO . OAuthException $ "Gaining OAuth Temporary Credential Failed: " ++ BSL.unpack (responseBody rsp)

-- | URL to obtain OAuth verifier.
authorizeUrl :: OAuth           -- ^ OAuth Application
             -> Credential      -- ^ Temporary Credential (Request Token & Secret)
             -> String          -- ^ URL to authorize
authorizeUrl oa cr = qsUrl (oauthAuthorizeUri oa) [("oauth_token", BS.unpack $ token cr)]

-- | Get Access token.
getAccessToken, getTokenCredential
               :: OAuth         -- ^ OAuth Application
               -> Credential    -- ^ Temporary Credential with oauth_verifier
               -> IO Credential -- ^ Token Credential (Access Token & Secret)
getAccessToken oa cr = do
  let req = (fromJust $ parseUrl $ oauthAccessTokenUri oa) { method = "POST" }
  rsp <- signOAuth oa cr req >>= withManager . httpLbs
  let dic = parseSimpleQuery . toStrict . responseBody $ rsp
  return $ Credential dic

getTokenCredential = getAccessToken

insertMap :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
insertMap key val = ((key,val):) . filter ((/=key).fst)

deleteMap :: Eq a => a -> [(a,b)] -> [(a,b)]
deleteMap k = filter ((/=k).fst)

-- | Insert an oauth parameter into given 'Credential'.
insert :: BS.ByteString -- ^ Parameter Name
       -> BS.ByteString -- ^ Value
       -> Credential    -- ^ Credential
       -> Credential    -- ^ Result
insert k v = Credential . insertMap k v . unCredential

-- | Convenient method for inserting multiple parameters into credential.
inserts :: [(BS.ByteString, BS.ByteString)] -> Credential -> Credential
inserts = flip $ foldr (uncurry insert)

-- | Remove an oauth parameter for key from given 'Credential'.
delete :: BS.ByteString -- ^ Parameter name
       -> Credential    -- ^ Credential
       -> Credential    -- ^ Result
delete key = Credential . deleteMap key . unCredential

-- | Add OAuth headers & sign to 'Request'.
signOAuth :: OAuth              -- ^ OAuth Application
          -> Credential         -- ^ Credential
          -> Request IO         -- ^ Original Request
          -> IO (Request IO)    -- ^ Signed OAuth Request
signOAuth oa crd req = do
  crd' <- addTimeStamp =<< addNonce crd
  let tok = injectOAuthToCred oa crd'
  sign <- genSign oa tok req
  return $ addAuthHeader (insert "oauth_signature" sign tok) req

baseTime :: UTCTime
baseTime = UTCTime day 0
  where
    day = ModifiedJulianDay 40587

showSigMtd :: SignMethod -> BS.ByteString
showSigMtd PLAINTEXT = "PLAINTEXT"
showSigMtd HMACSHA1  = "HMAC-SHA1"
showSigMtd (RSASHA1 _) = "RSA-SHA1"

addNonce :: Credential -> IO Credential
addNonce cred = do
  nonce <- replicateM 10 (randomRIO ('a','z'))
  return $ insert "oauth_nonce" (BS.pack nonce) cred

addTimeStamp :: Credential -> IO Credential
addTimeStamp cred = do
  stamp <- floor . (`diffUTCTime` baseTime) <$> getCurrentTime :: IO Integer
  return $ insert "oauth_timestamp" (BS.pack $ show stamp) cred

injectOAuthToCred :: OAuth -> Credential -> Credential
injectOAuthToCred oa cred = maybe id (insert "oauth_callback") (oauthCallback oa) $ 
  inserts [ ("oauth_signature_method", showSigMtd $ oauthSignatureMethod oa)
          , ("oauth_consumer_key", oauthConsumerKey oa)
          , ("oauth_version", "1.0")
          ] cred

genSign :: MonadIO m => OAuth -> Credential -> Request m -> m BS.ByteString
genSign oa tok req =
  case oauthSignatureMethod oa of
    HMACSHA1 -> do
      text <- getBaseString tok req
      let key  = BS.intercalate "&" $ map paramEncode [oauthConsumerSecret oa, tokenSecret tok]
      return $ encode $ toStrict $ bytestringDigest $ hmacSha1 (fromStrict key) text
    PLAINTEXT ->
      return $ BS.intercalate "&" $ map paramEncode [oauthConsumerSecret oa, tokenSecret tok]
    RSASHA1 pr ->
      liftM (encode . toStrict . rsassa_pkcs1_v1_5_sign ha_SHA1 pr) (getBaseString tok req)

addAuthHeader :: Credential -> Request a -> Request a
addAuthHeader (Credential cred) req =
  req { requestHeaders = insertMap "Authorization" (renderAuthHeader cred) $ requestHeaders req }

renderAuthHeader :: [(BS.ByteString, BS.ByteString)] -> BS.ByteString
renderAuthHeader = ("OAuth " `BS.append`). BS.intercalate "," . map (\(a,b) -> BS.concat [paramEncode a, "=\"",  paramEncode b, "\""]) . filter ((`notElem` ["oauth_token_secret", "oauth_consumer_secret"]) . fst)

-- | Encode a string using the percent encoding method for OAuth.
paramEncode :: BS.ByteString -> BS.ByteString
paramEncode = BS.concatMap escape
  where
    escape c | isAlpha c || isDigit c || c `elem` "-._~" = BS.singleton c
             | otherwise = let num = map toUpper $ showHex (ord c) ""
                               oct = '%' : replicate (2 - length num) '0' ++ num
                           in BS.pack oct

getBaseString :: MonadIO m => Credential -> Request m -> m BSL.ByteString
getBaseString tok req = do
  let bsMtd  = BS.map toUpper $ method req
      isHttps = secure req
      scheme = if isHttps then "https" else "http"
      bsPort = if (isHttps && port req /= 443) || (not isHttps && port req /= 80)
                 then ':' `BS.cons` BS.pack (show $ port req) else ""
      bsURI = BS.concat [scheme, "://", host req, bsPort, path req]
      bsQuery = map (second $ fromMaybe "") $ queryString req
  bsBodyQ <- if isBodyFormEncoded $ requestHeaders req
                  then liftM parseSimpleQuery $ toLBS (requestBody req)
                  else return []
  let bsAuthParams = filter ((`notElem`["oauth_signature","realm", "oauth_token_secret"]).fst) $ unCredential tok
      allParams = bsQuery++bsBodyQ++bsAuthParams
      bsParams = BS.intercalate "&" $ map (\(a,b)->BS.concat[a,"=",b]) $ sortBy compareTuple
                   $ map (\(a,b) -> (paramEncode a,paramEncode b)) allParams
  -- FIXME it would be much better to use http-types functions here
  return $ BSL.intercalate "&" $ map (fromStrict.paramEncode) [bsMtd, bsURI, bsParams]

toLBS :: MonadIO m => RequestBody m -> m BS.ByteString
toLBS (RequestBodyLBS l) = return $ toStrict l
toLBS (RequestBodyBS s) = return s
toLBS (RequestBodyBuilder _ b) = return $ toByteString b
toLBS (RequestBodyEnum _ enum) = do
    i <- liftIO $ newIORef id
    run_ $ enum $$ go i
    liftIO $ liftM (toByteString . mconcat . ($ [])) $ readIORef i
  where
    go i =
        continue go'
      where
        go' (Chunks []) = continue go'
        go' (Chunks x) = do
            liftIO (atomicModifyIORef i $ \y -> (y . (x ++), ()))
            continue go'
        go' EOF = return ()

isBodyFormEncoded :: [Header] -> Bool
isBodyFormEncoded = maybe False (=="application/x-www-form-urlencoded") . lookup "Content-Type"

compareTuple :: (Ord a, Ord b) => (a, b) -> (a, b) -> Ordering
compareTuple (a,b) (c,d) =
  case compare a c of
    LT -> LT
    EQ -> compare b d
    GT -> GT
