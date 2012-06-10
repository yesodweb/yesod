{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, StandaloneDeriving, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Web.Authenticate.OAuth
    ( -- * Data types
      OAuth, def, newOAuth, oauthServerName, oauthRequestUri, oauthAccessTokenUri,
      oauthAuthorizeUri, oauthSignatureMethod, oauthConsumerKey,
      oauthConsumerSecret, oauthCallback, oauthRealm, oauthVersion,
      OAuthVersion(..), SignMethod(..), Credential(..), OAuthException(..),
      -- * Operations for credentials
      newCredential, emptyCredential, insert, delete, inserts, injectVerifier,
      -- * Signature
      signOAuth, genSign,
      -- * Url & operation for authentication
      authorizeUrl, authorizeUrl', getAccessToken, getTemporaryCredential,
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
import Network.HTTP.Types (parseSimpleQuery, SimpleQuery)
import Control.Exception
import Control.Monad
import Data.List (sortBy)
import System.Random
import Data.Char
import Data.Digest.Pure.SHA
import Data.ByteString.Base64
import Data.Time
import Numeric
import Codec.Crypto.RSA (rsassa_pkcs1_v1_5_sign, ha_SHA1)
import Crypto.Types.PubKey.RSA (PrivateKey(..))
import Network.HTTP.Types (Header)
import Blaze.ByteString.Builder (toByteString)
import Control.Monad.IO.Class (MonadIO)
import Network.HTTP.Types (renderSimpleQuery, status200)
import Data.Conduit (($$), ($=), Source)
import qualified Data.Conduit.List as CL
import Data.Conduit.Blaze (builderToByteString)
import Blaze.ByteString.Builder (Builder)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Data.Default

-- | Data type for OAuth client (consumer).
-- 
-- The constructor for this data type is not exposed. 
-- Instead, you should use the 'def' method or 'newOAuth' function to retrieve a default instance, 
-- and then use the records below to make modifications.
-- This approach allows us to add configuration options without breaking backwards compatibility.
data OAuth = OAuth { oauthServerName      :: String -- ^ Service name (default: @\"\"@)
                   , oauthRequestUri      :: String
                   -- ^ URI to request temporary credential (default: @\"\"@).
                   --   You MUST specify if you use 'getTemporaryCredential'', 'getTemporaryCredentialProxy'
                   --   or 'getTemporaryCredential'; otherwise you can just leave this empty.
                   , oauthAccessTokenUri  :: String
                   -- ^ Uri to obtain access token (default: @\"\"@).
                   --   You MUST specify if you use 'getAcessToken' or 'getAccessToken'';
                   --   otherwise you can just leave this empty.
                   , oauthAuthorizeUri    :: String
                   -- ^ Uri to authorize (default: @\"\"@).
                   --   You MUST specify if you use 'authorizeUrl' or 'authorizeUrl'';
                   --   otherwise you can just leave this empty. 
                   , oauthSignatureMethod :: SignMethod
                   -- ^ Signature Method (default: 'HMACSHA1')
                   , oauthConsumerKey     :: BS.ByteString
                   -- ^ Consumer key (You MUST specify)
                   , oauthConsumerSecret  :: BS.ByteString
                   -- ^ Consumer Secret (You MUST specify)
                   , oauthCallback        :: Maybe BS.ByteString
                   -- ^ Callback uri to redirect after authentication (default: @Nothing@)
                   , oauthRealm           :: Maybe BS.ByteString
                   -- ^ Optional authorization realm (default: @Nothing@)
                   , oauthVersion         :: OAuthVersion
                   -- ^ OAuth spec version (default: 'OAuth10a')
                   } deriving (Show, Eq, Ord, Read, Data, Typeable)

data OAuthVersion = OAuth10     -- ^ OAuth protocol ver 1.0 (no oauth_verifier; differs from RFC 5849).
                  | OAuth10a    -- ^ OAuth protocol ver 1.0a. This corresponds to community's 1.0a spec and RFC 5849.
                    deriving (Show, Eq, Ord, Data, Typeable, Read)

-- | Default value for OAuth datatype.
-- You must specify at least oauthServerName, URIs and Tokens.
newOAuth :: OAuth
newOAuth = OAuth { oauthSignatureMethod = HMACSHA1
                 , oauthCallback = Nothing
                 , oauthRealm    = Nothing
                 , oauthServerName = ""
                 , oauthRequestUri = ""
                 , oauthAccessTokenUri = ""
                 , oauthAuthorizeUri = ""
                 , oauthConsumerKey = error "You MUST specify oauthConsumerKey parameter."
                 , oauthConsumerSecret = error "You MUST specify oauthConsumerSecret parameter."
                 , oauthVersion = OAuth10a
                 }

instance Default OAuth where
  def = newOAuth

-- | Data type for signature method.
data SignMethod = PLAINTEXT
                | HMACSHA1
                | RSASHA1 PrivateKey
                  deriving (Show, Eq, Ord, Read, Data, Typeable)
deriving instance Ord PrivateKey
-- | Data type for redential.
data Credential = Credential { unCredential :: [(BS.ByteString, BS.ByteString)] }
                  deriving (Show, Eq, Ord, Read, Data, Typeable)

-- | Empty credential.
emptyCredential :: Credential
emptyCredential = Credential []

-- | Convenient function to create 'Credential' with OAuth Token and Token Secret.
newCredential :: BS.ByteString -- ^ value for oauth_token
              -> BS.ByteString -- ^ value for oauth_token_secret
              -> Credential
newCredential tok sec = Credential [("oauth_token", tok), ("oauth_token_secret", sec)]

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
getTemporaryCredential :: (MonadResource m, MonadBaseControl IO m)
                       => OAuth         -- ^ OAuth Application
                       -> Manager
                       -> m Credential -- ^ Temporary Credential (Request Token & Secret).
getTemporaryCredential = getTemporaryCredential' id

-- | Get temporary credential for requesting access token with Scope parameter.
getTemporaryCredentialWithScope :: (MonadResource m, MonadBaseControl IO m)
                                => BS.ByteString -- ^ Scope parameter string
                                -> OAuth         -- ^ OAuth Application
                                -> Manager
                                -> m Credential -- ^ Temporay Credential (Request Token & Secret).
getTemporaryCredentialWithScope bs = getTemporaryCredential' (addScope bs)

addScope :: (MonadIO m) => BS.ByteString -> Request m -> Request m
addScope scope req | BS.null scope = req
                   | otherwise     = urlEncodedBody [("scope", scope)] req

-- | Get temporary credential for requesting access token via the proxy.
getTemporaryCredentialProxy :: (MonadResource m, MonadBaseControl IO m)
                            => Maybe Proxy   -- ^ Proxy
                            -> OAuth         -- ^ OAuth Application
                            -> Manager
                            -> m Credential -- ^ Temporary Credential (Request Token & Secret).
getTemporaryCredentialProxy p oa m = getTemporaryCredential' (addMaybeProxy p) oa m

getTemporaryCredential' :: (MonadResource m, MonadBaseControl IO m)
                        => (Request m -> Request m)   -- ^ Request Hook
                        -> OAuth                      -- ^ OAuth Application
                        -> Manager
                        -> m Credential    -- ^ Temporary Credential (Request Token & Secret).
getTemporaryCredential' hook oa manager = do
  let req = fromJust $ parseUrl $ oauthRequestUri oa
      crd = maybe id (insert "oauth_callback") (oauthCallback oa) $ emptyCredential
  req' <- signOAuth oa crd $ hook (req { method = "POST" }) 
  rsp <- httpLbs req' manager
  if responseStatus rsp == status200
    then do
      let dic = parseSimpleQuery . toStrict . responseBody $ rsp
      return $ Credential dic
    else liftIO . throwIO . OAuthException $ "Gaining OAuth Temporary Credential Failed: " ++ BSL.unpack (responseBody rsp)

-- | URL to obtain OAuth verifier.
authorizeUrl :: OAuth           -- ^ OAuth Application
             -> Credential      -- ^ Temporary Credential (Request Token & Secret)
             -> String          -- ^ URL to authorize
authorizeUrl = authorizeUrl' $ \oa -> const [("oauth_consumer_key", oauthConsumerKey oa)]

-- | Convert OAuth and Credential to URL to authorize.
--   This takes function to choice parameter to pass to the server other than
--   /oauth_callback/ or /oauth_token/.
authorizeUrl' :: (OAuth -> Credential -> SimpleQuery)
              -> OAuth           -- ^ OAuth Application
              -> Credential      -- ^ Temporary Credential (Request Token & Secret)
              -> String          -- ^ URL to authorize
authorizeUrl' f oa cr = oauthAuthorizeUri oa ++ BS.unpack (renderSimpleQuery True queries)
  where fixed   = ("oauth_token", token cr):f oa cr
        queries = 
          case oauthCallback oa of
            Nothing       -> fixed
            Just callback -> ("oauth_callback", callback):fixed


-- | Get Access token.
getAccessToken, getTokenCredential
               :: (MonadResource m, MonadBaseControl IO m)
               => OAuth         -- ^ OAuth Application
               -> Credential    -- ^ Temporary Credential (with oauth_verifier if >= 1.0a)
               -> Manager
               -> m Credential -- ^ Token Credential (Access Token & Secret)
getAccessToken = getAccessToken' id

-- | Get Access token via the proxy.
getAccessTokenProxy, getTokenCredentialProxy
               :: (MonadResource m, MonadBaseControl IO m)
               => Maybe Proxy   -- ^ Proxy
               -> OAuth         -- ^ OAuth Application
               -> Credential    -- ^ Temporary Credential (with oauth_verifier if >= 1.0a)
               -> Manager
               -> m Credential -- ^ Token Credential (Access Token & Secret)
getAccessTokenProxy p = getAccessToken' $ addMaybeProxy p

getAccessToken' :: (MonadResource m, MonadBaseControl IO m)
                => (Request m -> Request m)   -- ^ Request Hook
                -> OAuth                      -- ^ OAuth Application
                -> Credential                 -- ^ Temporary Credential (with oauth_verifier if >= 1.0a)
                -> Manager
                -> m Credential     -- ^ Token Credential (Access Token & Secret)
getAccessToken' hook oa cr manager = do
  let req = hook (fromJust $ parseUrl $ oauthAccessTokenUri oa) { method = "POST" }
  rsp <- flip httpLbs manager =<< signOAuth oa (if oauthVersion oa == OAuth10 then delete "oauth_verifier" cr else cr) req
  if responseStatus rsp == status200
    then do
      let dic = parseSimpleQuery . toStrict . responseBody $ rsp
      return $ Credential dic
    else liftIO . throwIO . OAuthException $ "Gaining OAuth Token Credential Failed: " ++ BSL.unpack (responseBody rsp)

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

injectVerifier :: BS.ByteString -> Credential -> Credential
injectVerifier = insert "oauth_verifier"

-- | Add OAuth headers & sign to 'Request'.
signOAuth :: (MonadUnsafeIO m)
          => OAuth              -- ^ OAuth Application
          -> Credential         -- ^ Credential
          -> Request m          -- ^ Original Request
          -> m (Request m)    -- ^ Signed OAuth Request
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

addNonce :: MonadUnsafeIO m => Credential -> m Credential
addNonce cred = do
  nonce <- unsafeLiftIO $ replicateM 10 (randomRIO ('a','z')) -- FIXME very inefficient
  return $ insert "oauth_nonce" (BS.pack nonce) cred

addTimeStamp :: MonadUnsafeIO m => Credential -> m Credential
addTimeStamp cred = do
  stamp <- (floor . (`diffUTCTime` baseTime)) `liftM` unsafeLiftIO getCurrentTime
  return $ insert "oauth_timestamp" (BS.pack $ show (stamp :: Integer)) cred

injectOAuthToCred :: OAuth -> Credential -> Credential
injectOAuthToCred oa cred =
    inserts [ ("oauth_signature_method", showSigMtd $ oauthSignatureMethod oa)
            , ("oauth_consumer_key", oauthConsumerKey oa)
            , ("oauth_version", "1.0")
            ] cred

genSign :: MonadUnsafeIO m => OAuth -> Credential -> Request m -> m BS.ByteString
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

getBaseString :: MonadUnsafeIO m => Credential -> Request m -> m BSL.ByteString
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

toLBS :: MonadUnsafeIO m => RequestBody m -> m BS.ByteString
toLBS (RequestBodyLBS l) = return $ toStrict l
toLBS (RequestBodyBS s) = return s
toLBS (RequestBodyBuilder _ b) = return $ toByteString b
toLBS (RequestBodySource _ src) = toLBS' src
toLBS (RequestBodySourceChunked src) = toLBS' src

toLBS' :: MonadUnsafeIO m => Source m Builder -> m BS.ByteString
toLBS' src = liftM BS.concat $ src $= builderToByteString $$ CL.consume

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
