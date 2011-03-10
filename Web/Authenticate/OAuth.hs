{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Web.Authenticate.OAuth
    ( -- * Data types
      OAuth(..), SignMethod(..), Credential(..),
      -- * Operations for credentials
      emptyCredential, insert, delete, inserts,
      -- * Signature
      signOAuth,
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
import Network.Wai.Parse
import Control.Exception
import Control.Monad
import Data.List (sortBy)
import System.Random
import Data.Char
import Data.Digest.Pure.SHA
import Data.ByteString.Base64
import Data.Time
import Numeric
import Network.Wai (ResponseHeader)
import Codec.Crypto.RSA (rsassa_pkcs1_v1_5_sign, ha_SHA1, PrivateKey(..))


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

data OAuthException = ProtocolException String
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
  let req = fromJust $ parseUrl (oauthRequestUri oa)
  req' <- signOAuth oa emptyCredential (req { method = "POST" }) 
  rsp <- httpLbs req'
  let dic = parseQueryString . toStrict . responseBody $ rsp
  return $ Credential dic

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
  rsp <- signOAuth oa cr req >>= httpLbs
  let dic = parseQueryString . toStrict . responseBody $ rsp
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
          -> Request            -- ^ Original Request
          -> IO Request         -- ^ Signed OAuth Request
signOAuth oa crd req = do
  crd' <- addTimeStamp =<< addNonce crd
  let tok = injectOAuthToCred oa crd'
      sign = genSign oa tok req
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

genSign :: OAuth -> Credential -> Request -> BS.ByteString
genSign oa tok req =
  case oauthSignatureMethod oa of
    HMACSHA1 ->
      let text = getBaseString tok req
          key  = BS.intercalate "&" $ map paramEncode [oauthConsumerSecret oa, tokenSecret tok]
      in encode $ toStrict $ bytestringDigest $ hmacSha1 (fromStrict key) text
    PLAINTEXT ->
      BS.intercalate "&" $ map paramEncode [oauthConsumerSecret oa, tokenSecret tok]
    RSASHA1 pr ->
      encode $ toStrict $ rsassa_pkcs1_v1_5_sign ha_SHA1 pr (getBaseString tok req)

addAuthHeader :: Credential -> Request -> Request
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

getBaseString :: Credential -> Request -> BSL.ByteString
getBaseString tok req =
  let bsMtd  = BS.map toUpper $ method req
      isHttps = secure req
      scheme = if isHttps then "https" else "http"
      bsPort = if (isHttps && port req /= 443) || (not isHttps && port req /= 80)
                 then ':' `BS.cons` BS.pack (show $ port req) else ""
      bsURI = BS.concat [scheme, "://", host req, bsPort, path req]
      bsQuery = queryString req
      bsBodyQ = if isBodyFormEncoded $ requestHeaders req
                  then parseQueryString (toStrict $ requestBody req) else []
      bsAuthParams = filter ((`notElem`["oauth_signature","realm", "oauth_token_secret"]).fst) $ unCredential tok
      allParams = bsQuery++bsBodyQ++bsAuthParams
      bsParams = BS.intercalate "&" $ map (\(a,b)->BS.concat[a,"=",b]) $ sortBy compareTuple
                   $ map (\(a,b) -> (paramEncode a,paramEncode b)) allParams
  in BSL.intercalate "&" $ map (fromStrict.paramEncode) [bsMtd, bsURI, bsParams]

isBodyFormEncoded :: [(ResponseHeader, BS.ByteString)] -> Bool
isBodyFormEncoded = maybe False (=="application/x-www-form-urlencoded") . lookup "Content-Type"

compareTuple :: (Ord a, Ord b) => (a, b) -> (a, b) -> Ordering
compareTuple (a,b) (c,d) =
  case compare a c of
    LT -> LT
    EQ -> compare b d
    GT -> GT
