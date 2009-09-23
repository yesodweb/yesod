{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
---------------------------------------------------------
--
-- Module        : Web.Restful.Request
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
module Web.Restful.Request
    (
      -- * Request parsing
      -- $param_overview

      -- ** Types
      ParamError
    , ParamName
    , ParamValue
      -- ** Parameter type class
    , Parameter (..)
      -- ** RequestParser helpers
    , getParam
    , postParam
    , urlParam
    , anyParam
    , cookieParam
    , identifier
    , acceptedLanguages
    , requestPath
      -- ** Building actual request
    , Request (..)
    , Hack.RequestMethod (..)
      -- ** FIXME
    , parseEnv
    , RawRequest (..)
    , PathInfo
    , runRequestParser
    ) where

import qualified Hack
import Data.Function.Predicate (equals)
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Error ()
import Web.Restful.Constants
import Web.Restful.Utils
import Control.Applicative (Applicative (..))
import Web.Encodings
import Data.Time.Calendar (Day, fromGregorian)
import Data.Char (isDigit)

-- $param_overview
-- In Restful, all of the underlying parameter values are strings. They can
-- come from multiple sources: GET parameters, URL rewriting (FIXME: link),
-- cookies, etc. However, most applications eventually want to convert
-- those strings into something else, like 'Int's. Additionally, it is
-- often desirable to allow multiple values, or no value at all.
--
-- That is what the parameter concept is for. A 'Parameter' is any value
-- which can be converted from a 'String', or list of 'String's.

-- | Any kind of error message generated in the parsing stage.
type ParamError = String

-- | In GET parameters, the key. In cookies, the cookie name. So on and so
-- forth.
type ParamName = String

-- | The 'String' value of a parameter, such as cookie content.
type ParamValue = String

-- | Anything which can be converted from a 'String' or list of 'String's.
--
-- The default implementation of 'readParams' will error out if given
-- anything but 1 'ParamValue'. This is usually what you want.
--
-- Minimal complete definition: either 'readParam' or 'readParams'.
class Parameter a where
    -- | Convert a string into the desired value, or explain why that can't
    -- happen.
    readParam :: ParamValue -> Either ParamError a
    readParam = readParams . return

    -- | Convert a list of strings into the desired value, or explain why
    -- that can't happen.
    readParams :: [ParamValue] -> Either ParamError a
    readParams [x] = readParam x
    readParams [] = Left "Missing parameter"
    readParams xs = Left $ "Given " ++ show (length xs) ++
                           " values, expecting 1"

-- | Attempt to parse a list of param values using 'readParams'.
-- If that fails, return an error message and an undefined value. This way,
-- we can process all of the parameters and get all of the error messages.
-- Be careful not to use the value inside until you can be certain the
-- reading succeeded.
tryReadParams:: Parameter a
             => ParamName
             -> [ParamValue]
             -> RequestParser a
tryReadParams name params =
    case readParams params of
        Left s -> do
            tell [(name, s)]
            return $
              error $
                "Trying to evaluate nonpresent parameter " ++
                name
        Right x -> return x

-- | Helper function for generating 'RequestParser's from various
-- 'ParamValue' lists.
genParam :: Parameter a
         => (RawRequest -> ParamName -> [ParamValue])
         -> ParamName
         -> RequestParser a
genParam f name = do
    req <- ask
    tryReadParams name $ f req name

-- | Parse a value passed as a GET parameter.
getParam :: Parameter a => ParamName -> RequestParser a
getParam = genParam getParams

-- | Parse a value passed as a POST parameter.
postParam :: Parameter a => ParamName -> RequestParser a
postParam = genParam postParams

-- | Parse a value passed in the URL and extracted using rewrite.
-- (FIXME: link to rewrite section.)
urlParam :: Parameter a => ParamName -> RequestParser a
urlParam = genParam urlParams

-- | Parse a value passed as a GET, POST or URL parameter.
anyParam :: Parameter a => ParamName -> RequestParser a
anyParam = genParam anyParams

-- | Parse a value passed as a raw cookie.
cookieParam :: Parameter a => ParamName -> RequestParser a
cookieParam = genParam cookies

-- | Parse a value in the hackHeader field.
hackHeaderParam :: Parameter a => ParamName -> RequestParser a
hackHeaderParam name = do
    env <- parseEnv
    let vals' = lookup name $ Hack.hackHeaders env
        vals = case vals' of
                Nothing -> []
                Just x -> [x]
    tryReadParams name vals

-- | Extract the cookie which specifies the identifier for a logged in
-- user.
identifier :: Parameter a => RequestParser a
identifier = hackHeaderParam authCookieName

-- | Get the raw 'Hack.Env' value.
parseEnv :: RequestParser Hack.Env
parseEnv = rawEnv `fmap` ask

-- | Determine the ordered list of language preferences.
--
-- FIXME: Future versions should account for some cookie.
acceptedLanguages :: RequestParser [String]
acceptedLanguages = do
    env <- parseEnv
    let rawLang = tryLookup "" "Accept-Language" $ Hack.http env
    return $! parseHttpAccept rawLang

-- | Determinge the path requested by the user (ie, the path info).
requestPath :: RequestParser String
requestPath = do
    env <- parseEnv
    let q = case Hack.queryString env of
                "" -> ""
                q'@('?':_) -> q'
                q' -> q'
    return $! Hack.pathInfo env ++ q

type RequestParser a = WriterT [(ParamName, ParamError)] (Reader RawRequest) a
instance Applicative (WriterT [(ParamName, ParamError)] (Reader RawRequest)) where
    pure = return
    (<*>) = ap

-- | Parse a request into either the desired 'Request' or a list of errors.
runRequestParser :: RequestParser a
                 -> RawRequest
                 -> Either [(ParamName, ParamError)] a
runRequestParser p req =
    let (val, errors) = (runReader (runWriterT p)) req
     in case errors of
            [] -> Right val
            x -> Left x

-- | The raw information passed through Hack, cleaned up a bit.
data RawRequest = RawRequest
    { rawPathInfo :: PathInfo
    , rawUrlParams :: [(ParamName, ParamValue)]
    , rawGetParams :: [(ParamName, ParamValue)]
    , rawPostParams :: [(ParamName, ParamValue)]
    , rawCookies :: [(ParamName, ParamValue)]
    , rawFiles :: [(ParamName, FileInfo)]
    , rawEnv :: Hack.Env
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
    readParams [x] = readParam x >>= return . Just
    readParams xs = Left $ "Given " ++ show (length xs) ++
                           " values, expecting 0 or 1"

instance Parameter a => Parameter [a] where
    readParams = mapM readParam

instance Parameter String where
    readParam = Right

instance Parameter Int where
    readParam s = case reads s of
                    ((x, _):_) -> Right x
                    _ -> Left $ "Invalid integer: " ++ s

instance Parameter Day where
    readParam s =
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

-- | The input for a resource.
--
-- Each resource can define its own instance of 'Request' and then more
-- easily ensure that it received the correct input (ie, correct variables,
-- properly typed).
class Request a where
    parseRequest :: RequestParser a

instance Request () where
    parseRequest = return ()

type PathInfo = [String]
