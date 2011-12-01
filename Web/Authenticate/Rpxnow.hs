{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
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
import Network.HTTP.Enumerator
import "transformers" Control.Monad.IO.Class
import Control.Failure
import Data.Maybe
import Control.Monad
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Exception (throwIO)
import Web.Authenticate.Internal
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Attoparsec.Lazy (parse)
import qualified Data.Attoparsec.Lazy as AT
import Data.Text (Text)
import qualified Data.Aeson.Types
#if MIN_VERSION_aeson(0, 4, 0)
import qualified Data.HashMap.Lazy as Map
#else
import qualified Data.Map as Map
#endif
import Control.Applicative ((<$>), (<*>))

-- | Information received from Rpxnow after a valid login.
data Identifier = Identifier
    { identifier :: Text
    , extraData :: [(Text, Text)]
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

-- | Attempt to log a user in.
authenticate :: (MonadIO m,
                 Failure HttpException m,
                 Failure AuthenticateException m)
             => String -- ^ API key given by RPXNOW.
             -> String -- ^ Token passed by client.
             -> m Identifier
authenticate apiKey token = do
    let body = L.fromChunks
            [ "apiKey="
            , S.pack apiKey
            , "&token="
            , S.pack token
            ]
    req' <- parseUrl "https://rpxnow.com"
    let req =
            req'
                { method = "POST"
                , path = "api/v2/auth_info"
                , requestHeaders =
                    [ ("Content-Type", "application/x-www-form-urlencoded")
                    ]
                , requestBody = RequestBodyLBS body
                }
    res <- liftIO $ withManager $ httpLbsRedirect req
    let b = responseBody res
    unless (200 <= statusCode res && statusCode res < 300) $
        liftIO $ throwIO $ StatusCodeException (statusCode res) b
    o <- unResult $ parse json b
    --m <- fromMapping o
    let mstat = flip Data.Aeson.Types.parse o $ \v ->
                case v of
                    Object m -> m .: "stat"
                    _ -> mzero
    case mstat of
        Success "ok" -> return ()
        Success stat -> failure $ RpxnowException $
            "Rpxnow login not accepted: " ++ stat ++ "\n" ++ L.unpack b
        _ -> failure $ RpxnowException "Now stat value found on Rpxnow response"
    case Data.Aeson.Types.parse parseProfile o of
        Success x -> return x
        Error e -> failure $ RpxnowException $ "Unable to parse Rpxnow response: " ++ e

unResult :: Failure AuthenticateException m => AT.Result a -> m a
unResult = either (failure . RpxnowException) return . AT.eitherResult

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
