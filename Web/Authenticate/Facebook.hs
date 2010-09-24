{-# LANGUAGE FlexibleContexts #-}
module Web.Authenticate.Facebook where

import Network.HTTP.Enumerator
import Data.List (intercalate)
import Data.Object
import Data.Object.Json
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S

data Facebook = Facebook
    { facebookClientId :: String
    , facebookClientSecret :: String
    , facebookRedirectUri :: String
    }
    deriving (Show, Eq, Read)

newtype AccessToken = AccessToken { unAccessToken :: String }
    deriving (Show, Eq, Read)

getForwardUrl :: Facebook -> [String] -> String
getForwardUrl fb perms = concat
    [ "https://graph.facebook.com/oauth/authorize?client_id="
    , facebookClientId fb -- FIXME escape
    , "&redirect_uri="
    , facebookRedirectUri fb -- FIXME escape
    , if null perms
        then ""
        else "&scope=" ++ intercalate "," perms
    ]

accessTokenUrl :: Facebook -> String -> String
accessTokenUrl fb code = concat
    [ "https://graph.facebook.com/oauth/access_token?client_id="
    , facebookClientId fb
    , "&redirect_uri="
    , facebookRedirectUri fb
    , "&client_secret="
    , facebookClientSecret fb
    , "&code="
    , code
    ]

getAccessToken :: Facebook -> String -> IO AccessToken
getAccessToken fb code = do
    let url = accessTokenUrl fb code
    b <- simpleHttp url
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

getGraphData :: AccessToken -> String -> IO StringObject
getGraphData at func = do
    let url = graphUrl at func
    b <- simpleHttp url
    decode $ S.concat $ L.toChunks b
