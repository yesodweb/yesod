{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Web.Authenticate.Facebook where

import Network.HTTP.Enumerator
import Data.List (intercalate)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8
import Web.Authenticate.Internal (qsEncode)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Control.Exception (Exception, throwIO)
import Data.Attoparsec.Lazy (parse, eitherResult)
import qualified Data.ByteString.Char8 as S8

data Facebook = Facebook
    { facebookClientId :: String
    , facebookClientSecret :: String
    , facebookRedirectUri :: String
    }
    deriving (Show, Eq, Read, Ord, Data, Typeable)

newtype AccessToken = AccessToken { unAccessToken :: String }
    deriving (Show, Eq, Read, Ord, Data, Typeable)

getForwardUrl :: Facebook -> [String] -> String
getForwardUrl fb perms = concat
    [ "https://graph.facebook.com/oauth/authorize?client_id="
    , qsEncode $ facebookClientId fb
    , "&redirect_uri="
    , qsEncode $ facebookRedirectUri fb
    , if null perms
        then ""
        else "&scope=" ++ qsEncode (intercalate "," perms)
    ]

accessTokenUrl :: Facebook -> String -> String
accessTokenUrl fb code = concat
    [ "https://graph.facebook.com/oauth/access_token?client_id="
    , qsEncode $ facebookClientId fb
    , "&redirect_uri="
    , qsEncode $ facebookRedirectUri fb
    , "&client_secret="
    , qsEncode $ facebookClientSecret fb
    , "&code="
    , qsEncode code
    ]

getAccessToken :: Facebook -> String -> IO AccessToken
getAccessToken fb code = do
    let url = accessTokenUrl fb code
    b <- simpleHttp $ S8.pack url
    let (front, back) = splitAt 13 $ L8.unpack b
    case front of
        "access_token=" -> return $ AccessToken back
        _ -> error $ "Invalid facebook response: " ++ back

graphUrl :: AccessToken -> String -> String
graphUrl (AccessToken s) func = concat
    [ "https://graph.facebook.com/"
    , func
    , "?access_token="
    , s
    ]

getGraphData :: AccessToken -> String -> IO (Either String Value)
getGraphData at func = do
    let url = graphUrl at func
    b <- simpleHttp $ S8.pack url
    return $ eitherResult $ parse json b

getGraphData' :: AccessToken -> String -> IO Value
getGraphData' a b = getGraphData a b >>= either (throwIO . InvalidJsonException) return

data InvalidJsonException = InvalidJsonException String
    deriving (Show, Typeable)
instance Exception InvalidJsonException
