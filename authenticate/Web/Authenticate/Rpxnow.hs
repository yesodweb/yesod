{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
---------------------------------------------------------
--
-- Module        : Web.Authenticate.Rpxnow
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Unstable
-- Portability   : portable
--
-- Facilitates authentication with "http://rpxnow.com/".
--
---------------------------------------------------------
module Web.Authenticate.Rpxnow
    ( Identifier (..)
    , authenticate
    , AuthenticateException (..)
    ) where

import Data.Aeson
import Network.HTTP.Conduit
import Control.Monad.IO.Class
import Data.Maybe
import Control.Monad
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Web.Authenticate.Internal
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Attoparsec.Lazy (parse)
import qualified Data.Attoparsec.Lazy as AT
import Data.Text (Text)
import qualified Data.Aeson.Types
import qualified Data.HashMap.Lazy as Map
import Control.Applicative ((<$>), (<*>))
import Control.Exception (throwIO)
import Data.Conduit (MonadBaseControl, MonadResource)

-- | Information received from Rpxnow after a valid login.
data Identifier = Identifier
    { identifier :: Text
    , extraData :: [(Text, Text)]
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

-- | Attempt to log a user in.
authenticate :: (MonadResource m, MonadBaseControl IO m)
             => String -- ^ API key given by RPXNOW.
             -> String -- ^ Token passed by client.
             -> Manager
             -> m Identifier
authenticate apiKey token manager = do
    let body = L.fromChunks
            [ "apiKey="
            , S.pack apiKey
            , "&token="
            , S.pack token
            ]
    req' <- liftIO $ parseUrl "https://rpxnow.com"
    let req =
            req'
                { method = "POST"
                , path = "api/v2/auth_info"
                , requestHeaders =
                    [ ("Content-Type", "application/x-www-form-urlencoded")
                    ]
                , requestBody = RequestBodyLBS body
                }
    res <- httpLbs req manager
    let b = responseBody res
    o <- unResult $ parse json b
    --m <- fromMapping o
    let mstat = flip Data.Aeson.Types.parse o $ \v ->
                case v of
                    Object m -> m .: "stat"
                    _ -> mzero
    case mstat of
        Success "ok" -> return ()
        Success stat -> liftIO $ throwIO $ RpxnowException $
            "Rpxnow login not accepted: " ++ stat ++ "\n" ++ L.unpack b
        _ -> liftIO $ throwIO $ RpxnowException "Now stat value found on Rpxnow response"
    case Data.Aeson.Types.parse parseProfile o of
        Success x -> return x
        Error e -> liftIO $ throwIO $ RpxnowException $ "Unable to parse Rpxnow response: " ++ e

unResult :: MonadIO m => AT.Result a -> m a
unResult = either (liftIO . throwIO . RpxnowException) return . AT.eitherResult

parseProfile :: Value -> Data.Aeson.Types.Parser Identifier
parseProfile (Object m) = do
    profile <- m .: "profile"
    Identifier
        <$> (profile .: "identifier")
        <*> return (mapMaybe go (Map.toList profile))
  where
    go ("identifier", _) = Nothing
    go (k, String v) = Just (k, v)
    go _ = Nothing
parseProfile _ = mzero
