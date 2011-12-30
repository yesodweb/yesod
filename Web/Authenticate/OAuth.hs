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
      getTokenCredential, getTemporaryCredentialWithScope,
      getAccessTokenProxy, getTemporaryCredentialProxy,
      getTokenCredentialProxy, 
      getAccessToken', getTemporaryCredential',
      -- * Utility Methods
      paramEncode, addScope, addMaybeProxy
    ) where
import Network.HTTP.Conduit
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
import Blaze.ByteString.Builder (toByteString)
import Control.Monad.IO.Class (MonadIO)
import Network.HTTP.Types (renderSimpleQuery)
import Data.Conduit (ResourceIO, runResourceT, ($$), ($=), Source)
import qualified Data.Conduit.List as CL
import Data.Conduit.Blaze (builderToByteString)
import Blaze.ByteString.Builder (Builder)

-- | Data type for OAuth client (consumer).
data OAuth = OAuth { oauthServerName      :: String        -- ^ Service name
                   , oauthRequestUri      :: String        -- ^ URI to request temporary credential
                   , oauthAccessTokenUri  :: String        -- ^ Uri to obtain access token
                   , oauthAuthorizeUri    :: String        -- ^ Uri to authorize
                   , oauthSignatureMethod :: SignMethod    -- ^ Signature Method
                   , oauthConsumerKey     :: BS.ByteString -- ^ Consumer key
                   , oauthConsumerSecret  :: BS.ByteString -- ^ Consumer Secret
                   , oauthCallback        :: Maybe BS.ByteString -- ^ Callback uri to redirect after authentication
                   , oauthRealm           :: Maybe BS.ByteString -- ^ Optional authorization realm
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
getTemporaryCredential = getTemporaryCredential' id

-- | Get temporary credential for requesting access token with Scope parameter.
getTemporaryCredentialWithScope :: BS.ByteString -- ^ Scope parameter string
                                -> OAuth         -- ^ OAuth Application
                                -> IO Credential -- ^ Temporay Credential (Request Token & Secret).
getTemporaryCredentialWithScope = getTemporaryCredential' . addScope

addScope :: (MonadIO m) => BS.ByteString -> Request m -> Request m
addScope scope req | BS.null scope = req
                   | otherwise     = urlEncodedBody [("scope", scope)] req

-- | Get temporary credential for requesting access token via the proxy.
getTemporaryCredentialProxy :: Maybe Proxy   -- ^ Proxy
                            -> OAuth         -- ^ OAuth Application
                            -> IO Credential -- ^ Temporary Credential (Request Token & Secret).
getTemporaryCredentialProxy p = getTemporaryCredential' $ addMaybeProxy p

getTemporaryCredential' :: (Request IO -> Request IO) -- ^ Request Hook
                        -> OAuth                      -- ^ OAuth Application
                        -> IO Credential              -- ^ Temporary Credential (Request Token & Secret).
getTemporaryCredential' hook oa = do
  let req = fromJust $ parseUrl $ oauthRequestUri oa
      crd = maybe id (insert "oauth_callback") (oauthCallback oa) $ emptyCredential
  req' <- signOAuth oa crd $ hook (req { method = "POST" }) 
  rsp <- withManager . httpLbs $ req'
  if statusCode rsp == 200
    then do
      let dic = parseSimpleQuery . toStrict . responseBody $ rsp
      return $ Credential dic
    else throwIO . OAuthException $ "Gaining OAuth Temporary Credential Failed: " ++ BSL.unpack (responseBody rsp)

-- | URL to obtain OAuth verifier.
authorizeUrl :: OAuth           -- ^ OAuth Application
             -> Credential      -- ^ Temporary Credential (Request Token & Secret)
             -> String          -- ^ URL to authorize
authorizeUrl oa cr = oauthAuthorizeUri oa ++ BS.unpack (renderSimpleQuery True queries)
  where queries = case oauthCallback oa of
                    Nothing       -> [("oauth_token", token cr)]
                    Just callback -> [("oauth_token", token cr), ("oauth_callback", callback)]

-- | Get Access token.
getAccessToken, getTokenCredential
               :: OAuth         -- ^ OAuth Application
               -> Credential    -- ^ Temporary Credential with oauth_verifier
               -> IO Credential -- ^ Token Credential (Access Token & Secret)
getAccessToken = getAccessToken' id

-- | Get Access token via the proxy.
getAccessTokenProxy, getTokenCredentialProxy
               :: Maybe Proxy   -- ^ Proxy
               -> OAuth         -- ^ OAuth Application
               -> Credential    -- ^ Temporary Credential with oauth_verifier
               -> IO Credential -- ^ Token Credential (Access Token & Secret)
getAccessTokenProxy p = getAccessToken' $ addMaybeProxy p

getAccessToken' :: (Request IO -> Request IO) -- ^ Request Hook
                -> OAuth                      -- ^ OAuth Application
                -> Credential                 -- ^ Temporary Credential with oauth_verifier
                -> IO Credential              -- ^ Token Credential (Access Token & Secret)
getAccessToken' hook oa cr = do
  let req = hook (fromJust $ parseUrl $ oauthAccessTokenUri oa) { method = "POST" }
  rsp <- withManager . httpLbs =<< signOAuth oa cr req
  if statusCode rsp == 200
    then do
      let dic = parseSimpleQuery . toStrict . responseBody $ rsp
      return $ Credential dic
    else throwIO . OAuthException $ "Gaining OAuth Token Credential Failed: " ++ BSL.unpack (responseBody rsp)


getTokenCredential = getAccessToken
getTokenCredentialProxy = getAccessTokenProxy

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
  return $ addAuthHeader prefix (insert "oauth_signature" sign tok) req
  where
    prefix = case oauthRealm oa of
      Nothing -> "OAuth "
      Just v  -> "OAuth realm=\"" `BS.append` v `BS.append` "\","

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
injectOAuthToCred oa cred =
    inserts [ ("oauth_signature_method", showSigMtd $ oauthSignatureMethod oa)
            , ("oauth_consumer_key", oauthConsumerKey oa)
            , ("oauth_version", "1.0")
            ] cred

genSign :: ResourceIO m => OAuth -> Credential -> Request m -> m BS.ByteString
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

addAuthHeader :: BS.ByteString -> Credential -> Request a -> Request a
addAuthHeader prefix (Credential cred) req =
  req { requestHeaders = insertMap "Authorization" (renderAuthHeader prefix cred) $ requestHeaders req }

renderAuthHeader :: BS.ByteString -> [(BS.ByteString, BS.ByteString)] -> BS.ByteString
renderAuthHeader prefix = (prefix `BS.append`). BS.intercalate "," . map (\(a,b) -> BS.concat [paramEncode a, "=\"",  paramEncode b, "\""]) . filter ((`elem` ["oauth_token", "oauth_verifier", "oauth_consumer_key", "oauth_signature_method", "oauth_timestamp", "oauth_nonce", "oauth_version", "oauth_callback", "oauth_signature"]) . fst)

-- | Encode a string using the percent encoding method for OAuth.
paramEncode :: BS.ByteString -> BS.ByteString
paramEncode = BS.concatMap escape
  where
    escape c | isAscii c && (isAlpha c || isDigit c || c `elem` "-._~") = BS.singleton c
             | otherwise = let num = map toUpper $ showHex (ord c) ""
                               oct = '%' : replicate (2 - length num) '0' ++ num
                           in BS.pack oct

getBaseString :: ResourceIO m => Credential -> Request m -> m BSL.ByteString
getBaseString tok req = do
  let bsMtd  = BS.map toUpper $ method req
      isHttps = secure req
      scheme = if isHttps then "https" else "http"
      bsPort = if (isHttps && port req /= 443) || (not isHttps && port req /= 80)
                 then ':' `BS.cons` BS.pack (show $ port req) else ""
      bsURI = BS.concat [scheme, "://", host req, bsPort, path req]
      bsQuery = parseSimpleQuery $ queryString req
  bsBodyQ <- if isBodyFormEncoded $ requestHeaders req
                  then liftM parseSimpleQuery $ toLBS (requestBody req)
                  else return []
  let bsAuthParams = filter ((`elem`["oauth_consumer_key","oauth_token", "oauth_version","oauth_signature_method","oauth_timestamp", "oauth_nonce", "oauth_verifier", "oauth_version","oauth_callback"]).fst) $ unCredential tok
      allParams = bsQuery++bsBodyQ++bsAuthParams
      bsParams = BS.intercalate "&" $ map (\(a,b)->BS.concat[a,"=",b]) $ sortBy compareTuple
                   $ map (\(a,b) -> (paramEncode a,paramEncode b)) allParams
  -- parameter encoding method in OAuth is slight different from ordinary one.
  -- So this is OK.
  return $ BSL.intercalate "&" $ map (fromStrict.paramEncode) [bsMtd, bsURI, bsParams]

toLBS :: ResourceIO m => RequestBody m -> m BS.ByteString
toLBS (RequestBodyLBS l) = return $ toStrict l
toLBS (RequestBodyBS s) = return s
toLBS (RequestBodyBuilder _ b) = return $ toByteString b
toLBS (RequestBodySource _ src) = toLBS' src
toLBS (RequestBodySourceChunked src) = toLBS' src

toLBS' :: ResourceIO m => Source m Builder -> m BS.ByteString
toLBS' src = fmap BS.concat $ runResourceT $ src $= builderToByteString $$ CL.consume

isBodyFormEncoded :: [Header] -> Bool
isBodyFormEncoded = maybe False (=="application/x-www-form-urlencoded") . lookup "Content-Type"

compareTuple :: (Ord a, Ord b) => (a, b) -> (a, b) -> Ordering
compareTuple (a,b) (c,d) =
  case compare a c of
    LT -> LT
    EQ -> compare b d
    GT -> GT

addMaybeProxy :: Maybe Proxy -> Request m -> Request m
addMaybeProxy p req = req { proxy = p }
