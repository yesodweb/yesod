module Hack.Middleware.ClientSession
    ( clientsession
    -- * Generating keys
    , Word256
    , defaultKeyFile
    , getKey
    , getDefaultKey
    ) where

import Prelude hiding (exp)
import Hack
import Web.Encodings
import Data.List (partition, intercalate)
import Data.Function.Predicate (is, isn't, equals)
import Data.Maybe (fromMaybe)
import Web.ClientSession
import Data.Time.Clock (getCurrentTime, UTCTime, addUTCTime)
import Data.Time.LocalTime () -- Show instance of UTCTime
import Data.Time.Format (formatTime) -- Read instance of UTCTime
import System.Locale (defaultTimeLocale)
import Control.Monad (guard)

-- | Automatic encrypting and decrypting of client session data.
--
-- Using the clientsession package, this middleware handles automatic
-- encryption, decryption, checking, expiration and renewal of whichever
-- cookies you ask it to. For example, if you tell it to deal with the
-- cookie \"IDENTIFIER\", it will do the following:
--
-- * When you specify an \"IDENTIFIER\" value in your 'Response', it will
-- encrypt the value, along with the session expiration date and the
-- REMOTE_HOST of the user. It will then be set as a cookie on the client.
--
-- * When there is an incoming \"IDENTIFIER\" cookie from the user, it will
-- decrypt it and check both the expiration date and the REMOTE_HOST. If
-- everything matches up, it will set the \"IDENTIFIER\" value in
-- 'hackHeaders'.
--
-- * If the client sent an \"IDENTIFIER\" and the application does not set
-- a new value, this will reset the cookie to a new expiration date. This
-- way, you do not have sessions timing out every 20 minutes.
--
-- As far as security: clientsesion itself handles hashing and encrypting
-- the data to make sure that the user can neither see not tamper with it.
clientsession :: [String] -- ^ list of cookies to intercept
              -> Word256 -- ^ encryption key
              -> Middleware
clientsession cnames key app env = do
    let initCookiesRaw :: String
        initCookiesRaw = fromMaybe "" $ lookup "Cookie" $ http env
        nonCookies :: [(String, String)]
        nonCookies = filter (fst `isn't` (== "Cookie")) $ http env
        initCookies :: [(String, String)]
        initCookies = decodeCookies initCookiesRaw
        cookies, interceptCookies :: [(String, String)]
        (interceptCookies, cookies) = partition (fst `is` (`elem` cnames))
                                      initCookies
        cookiesRaw :: String
        cookiesRaw = intercalate "; " $ map (\(k, v) -> k ++ "=" ++ v)
                     cookies
        remoteHost :: String
        remoteHost = fromMaybe "" $ lookup "REMOTE_HOST" $ http env
    now <- getCurrentTime
    let convertedCookies =
            takeJusts $
            map (decodeCookie key now remoteHost) interceptCookies
    let env' = env { http = ("Cookie", cookiesRaw)
                            : filter (fst `equals` "Cookie") (http env)
                            ++ nonCookies
                   , hackHeaders = hackHeaders env ++ convertedCookies
                   }
    res <- app env'
    let (interceptHeaders, headers') = partition (fst `is` (`elem` cnames))
                                     $ headers res
    let twentyMinutes :: Int
        twentyMinutes = 20 * 60
    let exp = fromIntegral twentyMinutes `addUTCTime` now
    let formattedExp = formatTime defaultTimeLocale "%a, %d-%b-%Y %X %Z" exp
    let oldCookies = filter (\(k, _) -> not $ k `elem` map fst interceptHeaders) convertedCookies
    let newCookies = map (setCookie key exp formattedExp remoteHost) $
                     oldCookies ++ interceptHeaders
    let res' = res { headers = newCookies ++ headers' }
    return res'

takeJusts :: [Maybe a] -> [a]
takeJusts [] = []
takeJusts (Just x:rest) = x : takeJusts rest
takeJusts (Nothing:rest) = takeJusts rest

setCookie :: Word256
          -> UTCTime -- ^ expiration time
          -> String -- ^ formatted expiration time
          -> String -- ^ remote host
          -> (String, String) -> (String, String)
setCookie key exp fexp rhost (cname, cval) =
    ("Set-Cookie", cname ++ "=" ++ val ++ "; path=/; expires=" ++ fexp)
      where
        val = encrypt key $ show $ Cookie exp rhost cval

data Cookie = Cookie UTCTime String String deriving (Show, Read)
decodeCookie :: Word256 -- ^ key
             -> UTCTime -- ^ current time
             -> String -- ^ remote host field
             -> (String, String) -- ^ cookie pair
             -> Maybe (String, String)
decodeCookie key now rhost (cname, encrypted) = do
    decrypted <- decrypt key encrypted
    (Cookie exp rhost' val) <- mread decrypted
    guard $ exp > now
    guard $ rhost' == rhost
    guard $ val /= ""
    return (cname, val)

mread :: (Monad m, Read a) => String -> m a
mread s =
    case reads s of
        [] -> fail $ "Reading of " ++ s ++ " failed"
        ((x, _):_) -> return x
