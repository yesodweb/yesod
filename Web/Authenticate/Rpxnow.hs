{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
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
    ) where

import Data.Object
import Data.Object.Json
import Network.HTTP.Wget
import "transformers" Control.Monad.IO.Class
import Control.Failure
import Data.Maybe
import Web.Authenticate.OpenId (AuthenticateException (..))
import Control.Monad
import Data.ByteString.Char8 (pack)

-- | Information received from Rpxnow after a valid login.
data Identifier = Identifier
    { identifier :: String
    , extraData :: [(String, String)]
    }

-- | Attempt to log a user in.
authenticate :: (MonadIO m,
                 Failure WgetException m,
                 Failure AuthenticateException m,
                 Failure ObjectExtractError m,
                 Failure JsonDecodeError m)
             => String -- ^ API key given by RPXNOW.
             -> String -- ^ Token passed by client.
             -> m Identifier
authenticate apiKey token = do
    b <- wget "https://rpxnow.com/api/v2/auth_info"
              []
              [ ("apiKey", apiKey)
              , ("token", token)
              ]
    o <- decode $ pack b
    m <- fromMapping o
    stat <- lookupScalar "stat" m
    unless (stat == "ok") $ failure $ AuthenticateException $
        "Rpxnow login not accepted: " ++ stat ++ "\n" ++ b
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
