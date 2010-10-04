{-# LANGUAGE FlexibleContexts #-}
module Web.Authenticate.Facebook where

import Network.HTTP.Enumerator
import Data.List (intercalate)
import Data.Object
import Data.Object.Json
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import qualified Codec.Binary.UTF8.String
import Numeric (showHex)

data Facebook = Facebook
    { facebookClientId :: String
    , facebookClientSecret :: String
    , facebookRedirectUri :: String
    }
    deriving (Show, Eq, Read)

newtype AccessToken = AccessToken { unAccessToken :: String }
    deriving (Show, Eq, Read)

qsEncode :: String -> String
qsEncode =
    concatMap go . Codec.Binary.UTF8.String.encode
  where
    go 32 = "+" -- space
    go 46 = "."
    go 45 = "-"
    go 126 = "~"
    go 95 = "_"
    go c
        | 48 <= c && c <= 57 = [w2c c]
        | 65 <= c && c <= 90 = [w2c c]
        | 97 <= c && c <= 122 = [w2c c]
    go c = '%' : showHex c ""
    w2c = toEnum . fromEnum

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
