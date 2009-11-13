{-# LANGUAGE FlexibleContexts #-}
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

import Text.JSON -- FIXME use Data.Object.JSON
import Network.HTTP.Wget
import Data.Maybe (isJust, fromJust)
import Control.Monad.Trans
import Control.Monad.Failure

-- | Information received from Rpxnow after a valid login.
data Identifier = Identifier
    { identifier :: String
    , extraData :: [(String, String)]
    }

-- | Attempt to log a user in.
authenticate :: (MonadIO m, MonadFailure WgetException m, MonadFailure StringException m)
             => String -- ^ API key given by RPXNOW.
             -> String -- ^ Token passed by client.
             -> m Identifier
authenticate apiKey token = do
  b <- wget
                "https://rpxnow.com/api/v2/auth_info"
                []
                [ ("apiKey", apiKey)
                , ("token", token)
                ]
  case decode b >>= getObject of
    Error s -> failureString $ "Not a valid JSON response: " ++ s -- FIXME
    Ok o ->
      case valFromObj "stat" o of
        Error _ -> failureString "Missing 'stat' field"
        Ok "ok" -> parseProfile o
        Ok stat -> failureString $ "Login not accepted: " ++ stat
                   ++ "\n" ++ b

parseProfile :: Monad m => JSObject JSValue -> m Identifier
parseProfile v = do
    profile <- resultToMonad $ valFromObj "profile" v >>= getObject
    ident <- resultToMonad $ valFromObj "identifier" profile
    let pairs = fromJSObject profile
        pairs' = filter (\(k, _) -> k /= "identifier") pairs
        pairs'' = map fromJust . filter isJust . map takeString $ pairs'
    return $ Identifier ident pairs''

takeString :: (String, JSValue) -> Maybe (String, String)
takeString (k, JSString v) = Just (k, fromJSString v)
takeString _ = Nothing

getObject :: Monad m => JSValue -> m (JSObject JSValue)
getObject (JSObject o) = return o
getObject _ = fail "Not an object"

resultToMonad :: Monad m => Result a -> m a
resultToMonad (Ok x) = return x
resultToMonad (Error s) = fail s
