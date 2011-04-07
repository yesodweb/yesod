{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Authenticate.Facebook
    ( Facebook (..)
    , AccessToken (..)
    , getForwardUrl
    , getAccessToken
    , getGraphData
    ) where

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
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Blaze.ByteString.Builder (toByteString, copyByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import Network.HTTP.Types (renderQueryText)
import Data.Monoid (mappend)
import Data.ByteString (ByteString)

data Facebook = Facebook
    { facebookClientId :: Text
    , facebookClientSecret :: Text
    , facebookRedirectUri :: Text
    }
    deriving (Show, Eq, Read, Ord, Data, Typeable)

newtype AccessToken = AccessToken { unAccessToken :: Text }
    deriving (Show, Eq, Read, Ord, Data, Typeable)

getForwardUrl :: Facebook -> [Text] -> Text
getForwardUrl fb perms =
    TE.decodeUtf8 $ toByteString $
    copyByteString "https://graph.facebook.com/oauth/authorize"
    `mappend`
    renderQueryText True
        ( ("client_id", Just $ facebookClientId fb)
        : ("redirect_uri", Just $ facebookRedirectUri fb)
        : if null perms
            then []
            else [("scope", Just $ T.intercalate "," perms)])


accessTokenUrl :: Facebook -> Text -> ByteString
accessTokenUrl fb code =
    toByteString $
    copyByteString "https://graph.facebook.com/oauth/access_token"
    `mappend`
    renderQueryText True
        [ ("client_id", Just $ facebookClientId fb)
        , ("redirect_uri", Just $ facebookRedirectUri fb)
        , ("code", Just code)
        ]

getAccessToken :: Facebook -> Text -> IO AccessToken
getAccessToken fb code = do
    let url = accessTokenUrl fb code
    b <- simpleHttp $ S8.unpack url
    let (front, back) = splitAt 13 $ L8.unpack b
    case front of
        "access_token=" -> return $ AccessToken $ T.pack back
        _ -> error $ "Invalid facebook response: " ++ back

graphUrl :: AccessToken -> Text -> ByteString
graphUrl (AccessToken s) func =
    toByteString $
    copyByteString "https://graph.facebook.com/"
    `mappend` fromText func
    `mappend` renderQueryText True [("access_token", Just s)]

getGraphData :: AccessToken -> Text -> IO (Either String Value)
getGraphData at func = do
    let url = graphUrl at func
    b <- simpleHttp $ S8.unpack url
    return $ eitherResult $ parse json b

getGraphData' :: AccessToken -> Text -> IO Value
getGraphData' a b = getGraphData a b >>= either (throwIO . InvalidJsonException) return

data InvalidJsonException = InvalidJsonException String
    deriving (Show, Typeable)
instance Exception InvalidJsonException
