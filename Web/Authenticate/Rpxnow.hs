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

import Text.JSON
import Network.HTTP.Wget
import Data.Maybe (isJust, fromJust)

-- | Information received from Rpxnow after a valid login.
data Identifier = Identifier
    { identifier :: String
    , extraData :: [(String, String)]
    }

-- | Attempt to log a user in.
authenticate :: Monad m
             => String -- ^ API key given by RPXNOW.
             -> String -- ^ Token passed by client.
             -> IO (m Identifier)
authenticate apiKey token = do
    body <- wget
                "https://rpxnow.com/api/v2/auth_info"
                []
                [ ("apiKey", apiKey)
                , ("token", token)
                ]
    case body of
        Left s -> return $ fail $ "Unable to connect to rpxnow: " ++ s
        Right b ->
          case decode b >>= getObject of
            Error s -> return $ fail $ "Not a valid JSON response: " ++ s
            Ok o ->
              case valFromObj "stat" o of
                Error _ -> return $ fail "Missing 'stat' field"
                Ok "ok" -> return $ parseProfile o
                Ok stat -> return $ fail $ "Login not accepted: " ++ stat

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
