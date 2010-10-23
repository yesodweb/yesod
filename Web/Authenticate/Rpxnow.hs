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

import Data.Object
import Data.Object.Json
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

-- | Information received from Rpxnow after a valid login.
data Identifier = Identifier
    { identifier :: String
    , extraData :: [(String, String)]
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

-- | Attempt to log a user in.
authenticate :: (MonadIO m,
                 Failure HttpException m,
                 Failure AuthenticateException m,
                 Failure ObjectExtractError m,
                 Failure JsonDecodeError m)
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
    let req =
            Request
                { method = "POST"
                , secure = True
                , host = "rpxnow.com"
                , port = 443
                , path = "api/v2/auth_info"
                , queryString = []
                , requestHeaders =
                    [ ("Content-Type", "application/x-www-form-urlencoded")
                    ]
                , requestBody = body
                }
    res <- httpLbsRedirect req
    let b = responseBody res
    unless (200 <= statusCode res && statusCode res < 300) $
        liftIO $ throwIO $ StatusCodeException (statusCode res) b
    o <- decode $ S.concat $ L.toChunks b
    m <- fromMapping o
    stat <- lookupScalar "stat" m
    unless (stat == "ok") $ failure $ RpxnowException $
        "Rpxnow login not accepted: " ++ stat ++ "\n" ++ L.unpack b
    parseProfile m

parseProfile :: (Monad m, Failure ObjectExtractError m)
             => [(String, StringObject)] -> m Identifier
parseProfile m = do
    profile <- lookupMapping "profile" m
    ident <- lookupScalar "identifier" profile
    let profile' = mapMaybe go profile
    return $ Identifier ident profile'
  where
    go ("identifier", _) = Nothing
    go (k, Scalar v) = Just (k, v)
    go _ = Nothing
