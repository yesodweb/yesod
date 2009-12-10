{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
---------------------------------------------------------
--
-- Module        : Yesod.Request
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
-- Code for extracting parameters from requests.
--
---------------------------------------------------------
module Yesod.Request
    (
      -- * Parameter
      -- $param_overview
      Parameter (..)
    , ParamError
    , ParamType
    , ParamName
    , ParamValue
    , RawParam (..)
      -- * RawRequest
    , RawRequest (..)
    , PathInfo
      -- * Parameter type class
      -- * MonadRequestReader type class and helpers
    , MonadRequestReader (..)
    , getParam
    , postParam
    , urlParam
    , anyParam
    , cookieParam
    , identifier
    , maybeIdentifier
    , acceptedLanguages
    , requestPath
    , parseEnv
    , approot
      -- * Building actual request
    , Request (..)
    , Hack.RequestMethod (..)
      -- * Parameter restrictions
    , notBlank
    ) where

import qualified Hack
import Data.Function.Predicate (equals)
import Yesod.Constants
import Yesod.Utils
import Control.Applicative (Applicative (..))
import Web.Encodings
import Data.Time.Calendar (Day, fromGregorian)
import Data.Char (isDigit)
import Data.Object.Translate (Language)
import qualified Data.ByteString.Lazy as BL

-- $param_overview
-- In Restful, all of the underlying parameter values are strings. They can
-- come from multiple sources: GET parameters, URL rewriting (FIXME: link),
-- cookies, etc. However, most applications eventually want to convert
-- those strings into something else, like 'Int's. Additionally, it is
-- often desirable to allow multiple values, or no value at all.
--
-- That is what the parameter concept is for. A 'Parameter' is any value
-- which can be converted from a 'String', or list of 'String's.

-- | Where this parameter came from.
data ParamType =
    GetParam
    | PostParam
    | UrlParam
    | CookieParam
    deriving (Eq, Show)

-- | Any kind of error message generated in the parsing stage.
type ParamError = String

-- | In GET parameters, the key. In cookies, the cookie name. So on and so
-- forth.
type ParamName = String

-- | The 'String' value of a parameter, such as cookie content.
type ParamValue = String

data RawParam = RawParam
    { paramType :: ParamType
    , paramName :: ParamName
    , paramValue :: ParamValue
    }

-- | Anything which can be converted from a 'String' or list of 'String's.
--
-- The default implementation of 'readParams' will error out if given
-- anything but 1 'ParamValue'. This is usually what you want.
--
-- Minimal complete definition: either 'readParam' or 'readParams'.
class Parameter a where
    -- | Convert a string into the desired value, or explain why that can't
    -- happen.
    readParam :: RawParam -> Either ParamError a
    readParam = readParams . return

    -- | Convert a list of strings into the desired value, or explain why
    -- that can't happen.
    readParams :: [RawParam] -> Either ParamError a
    readParams [x] = readParam x
    readParams [] = Left "Missing parameter"
    readParams xs = Left $ "Given " ++ show (length xs) ++
                           " values, expecting 1"

instance Parameter RawParam where
    readParam = Right

class (Monad m, Functor m, Applicative m) => MonadRequestReader m where
    askRawRequest :: m RawRequest
    invalidParam :: ParamType -> ParamName -> ParamError -> m a
    authRequired :: m a

-- | Attempt to parse a list of param values using 'readParams'.
-- If that fails, return an error message and an undefined value. This way,
-- we can process all of the parameters and get all of the error messages.
-- Be careful not to use the value inside until you can be certain the
-- reading succeeded.
tryReadParams:: (Parameter a, MonadRequestReader m)
             => ParamType
             -> ParamName
             -> [RawParam]
             -> m a
tryReadParams ptype name params =
    case readParams params of
        Left s -> invalidParam ptype name s
        Right x -> return x

-- | Helper function for generating 'RequestParser's from various
-- 'ParamValue' lists.
genParam :: (Parameter a, MonadRequestReader m)
         => (RawRequest -> ParamName -> [ParamValue])
         -> ParamType
         -> ParamName
         -> m a
genParam f ptype name = do
    req <- askRawRequest
    tryReadParams ptype name $ map (RawParam ptype name) $ f req name

-- | Parse a value passed as a GET parameter.
getParam :: (Parameter a, MonadRequestReader m) => ParamName -> m a
getParam = genParam getParams GetParam

-- | Parse a value passed as a POST parameter.
postParam :: (Parameter a, MonadRequestReader m) => ParamName -> m a
postParam = genParam postParams PostParam

-- | Parse a value passed in the URL and extracted using rewrite.
-- (FIXME: link to rewrite section.)
urlParam :: (Parameter a, MonadRequestReader m) => ParamName -> m a
urlParam = genParam urlParams UrlParam

-- | Parse a value passed as a GET, POST or URL parameter.
anyParam :: (Parameter a, MonadRequestReader m) => ParamName -> m a
anyParam = genParam anyParams PostParam -- FIXME

-- | Parse a value passed as a raw cookie.
cookieParam :: (Parameter a, MonadRequestReader m) => ParamName -> m a
cookieParam = genParam cookies CookieParam

-- | Extract the cookie which specifies the identifier for a logged in
-- user.
identifier :: MonadRequestReader m => m String
identifier = do
    mi <- maybeIdentifier
    case mi of
        Nothing -> authRequired
        Just x -> return x

-- | Extract the cookie which specifies the identifier for a logged in
-- user, if available.
maybeIdentifier :: MonadRequestReader m => m (Maybe String)
maybeIdentifier = do
    env <- parseEnv
    case lookup authCookieName $ Hack.hackHeaders env of
        Nothing -> return Nothing
        Just x -> return (Just x)

-- | Get the raw 'Hack.Env' value.
parseEnv :: MonadRequestReader m => m Hack.Env
parseEnv = rawEnv `fmap` askRawRequest

-- | The URL to the application root (ie, the URL with pathInfo /).
approot :: MonadRequestReader m => m String
approot = do
    env <- parseEnv
    let (scheme, defPort) =
            case Hack.hackUrlScheme env of
                Hack.HTTP -> ("http://", 80)
                Hack.HTTPS -> ("https://", 443)
    let sn = Hack.serverName env
    let portSuffix =
            if Hack.serverPort env == defPort
                then ""
                else ':' : show (Hack.serverPort env)
    return $! scheme ++ sn ++ portSuffix ++ "/"

-- | Determine the ordered list of language preferences.
--
-- FIXME: Future versions should account for some cookie.
acceptedLanguages :: MonadRequestReader m => m [String]
acceptedLanguages = do
    env <- parseEnv
    let rawLang = tryLookup "" "Accept-Language" $ Hack.http env
    return $! parseHttpAccept rawLang

-- | Determinge the path requested by the user (ie, the path info).
requestPath :: MonadRequestReader m => m String
requestPath = do
    env <- parseEnv
    let q = case Hack.queryString env of
                "" -> ""
                q'@('?':_) -> q'
                q' -> q'
    return $! Hack.pathInfo env ++ q

type PathInfo = [String]

-- | The raw information passed through Hack, cleaned up a bit.
data RawRequest = RawRequest
    { rawPathInfo :: PathInfo
    , rawUrlParams :: [(ParamName, ParamValue)]
    , rawGetParams :: [(ParamName, ParamValue)]
    , rawPostParams :: [(ParamName, ParamValue)]
    , rawCookies :: [(ParamName, ParamValue)]
    , rawFiles :: [(ParamName, FileInfo String BL.ByteString)]
    , rawEnv :: Hack.Env
    , rawLanguages :: [Language]
    }
    deriving Show

-- | All GET paramater values with the given name.
getParams :: RawRequest -> ParamName -> [ParamValue]
getParams rr name = map snd
                  . filter (\x -> name == fst x)
                  . rawGetParams
                  $ rr

-- | All POST paramater values with the given name.
postParams :: RawRequest -> ParamName -> [ParamValue]
postParams rr name = map snd
                   . filter (\x -> name == fst x)
                   . rawPostParams
                   $ rr

-- | All URL paramater values (see rewriting) with the given name.
urlParams :: RawRequest -> ParamName -> [ParamValue]
urlParams rr name = map snd
                  . filter (\x -> name == fst x)
                  . rawUrlParams
                  $ rr

-- | All GET, POST and URL paramater values (see rewriting) with the given name.
anyParams :: RawRequest -> ParamName -> [ParamValue]
anyParams req name = urlParams req name ++
                     getParams req name ++
                     postParams req name

-- | All cookies with the given name.
cookies :: RawRequest -> ParamName -> [ParamValue]
cookies rr name = map snd . filter (fst `equals` name) . rawCookies $ rr

instance Parameter a => Parameter (Maybe a) where
    readParams [] = Right Nothing
    readParams [x] = Just `fmap` readParam x
    readParams xs = Left $ "Given " ++ show (length xs) ++
                           " values, expecting 0 or 1"

instance Parameter a => Parameter [a] where
    readParams = mapM' readParam where
        mapM' f = sequence' . map f
        sequence' :: [Either String v] -> Either String [v]
        sequence' [] = Right []
        sequence' (Left l:_) = Left l
        sequence' (Right r:rest) =
            case sequence' rest of
                Left l -> Left l
                Right rest' -> Right $ r : rest'

instance Parameter [Char] where
    readParam = Right . paramValue

instance Parameter Int where
    readParam (RawParam _ _ s) = case reads s of
                    ((x, _):_) -> Right x
                    _ -> Left $ "Invalid integer: " ++ s

instance Parameter Day where
    readParam (RawParam _ _ s) =
        let t1 = length s == 10
            t2 = s !! 4 == '-'
            t3 = s !! 7 == '-'
            t4 = all isDigit $ concat
                    [ take 4 s
                    , take 2 $ drop 5 s
                    , take 2 $ drop 8 s
                    ]
            t = and [t1, t2, t3, t4]
            y = read $ take 4 s
            m = read $ take 2 $ drop 5 s
            d = read $ take 2 $ drop 8 s
         in if t
                then Right $ fromGregorian y m d
                else Left $ "Invalid date: " ++ s

-- for checkboxes; checks for presence
instance Parameter Bool where
    readParams [] = Right False
    readParams [_] = Right True
    readParams x = Left $ "Invalid Bool parameter: " ++ show (map paramValue x)

-- | The input for a resource.
--
-- Each resource can define its own instance of 'Request' and then more
-- easily ensure that it received the correct input (ie, correct variables,
-- properly typed).
class Request a where
    parseRequest :: MonadRequestReader m => m a

instance Request () where
    parseRequest = return ()

-- | Ensures that a String parameter is not blank.
notBlank :: MonadRequestReader m => RawParam -> m String
notBlank rp =
  case paramValue rp of
    "" -> invalidParam (paramType rp) (paramName rp) "Required field"
    s -> return s
