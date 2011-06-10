{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Authenticate.Facebook
    ( Facebook (..)
    , AccessToken (..)
    , getForwardUrl
    , getAccessToken
    , getGraphData
    , getGraphData_
    ) where

import Network.HTTP.Enumerator
import Network.HTTP.Types (parseSimpleQuery)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Data (Data)
import Data.Typeable (Typeable)
import Control.Exception (Exception, throwIO)
import Data.Attoparsec.Lazy (parse, eitherResult)
import qualified Data.ByteString.Char8 as S8
import Data.Text (Text)
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
        , ("client_secret", Just $ facebookClientSecret fb)
        ]

getAccessToken :: Facebook -> Text -> IO AccessToken
getAccessToken fb code = do
    let url = accessTokenUrl fb code
    b <- simpleHttp $ S8.unpack url
    let params = parseSimpleQuery $ S8.concat $ L8.toChunks b
    case lookup "access_token" params of
        Just x -> return $ AccessToken $ T.pack $ S8.unpack x
        Nothing -> error $ "Invalid facebook response: " ++ L8.unpack b

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

getGraphData_ :: AccessToken -> Text -> IO Value
getGraphData_ a b = getGraphData a b >>= either (throwIO . InvalidJsonException) return

data InvalidJsonException = InvalidJsonException String
    deriving (Show, Typeable)
instance Exception InvalidJsonException
