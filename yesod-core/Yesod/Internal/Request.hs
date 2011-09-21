{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Yesod.Internal.Request
    ( parseWaiRequest
    , Request (..)
    , RequestBodyContents
    , FileInfo (..)
    -- The below are exported for testing.
    , randomString
    , parseWaiRequest'
    ) where

import Control.Applicative ((<$>))
import Control.Arrow (second)
import qualified Network.Wai.Parse as NWP
import Yesod.Internal
import qualified Network.Wai as W
import System.Random (RandomGen, newStdGen, randomRs)
import Web.Cookie (parseCookiesText)
import qualified Data.ByteString.Char8 as S8
import Data.Text (Text, pack)
import Network.HTTP.Types (queryToQueryText)
import Control.Monad (join)
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.ByteString.Lazy as L

-- | The parsed request information.
data Request = Request
    { reqGetParams :: [(Text, Text)]
    , reqCookies :: [(Text, Text)]
    , reqWaiRequest :: W.Request
      -- | Languages which the client supports.
    , reqLangs :: [Text]
      -- | A random, session-specific nonce used to prevent CSRF attacks.
    , reqNonce :: Maybe Text
    }

parseWaiRequest :: W.Request
                -> [(Text, Text)] -- ^ session
                -> Maybe a
                -> IO Request
parseWaiRequest env session' key' = parseWaiRequest' env session' key' <$> newStdGen

parseWaiRequest' :: RandomGen g
                 => W.Request
                 -> [(Text, Text)] -- ^ session
                 -> Maybe a
                 -> g
                 -> Request
parseWaiRequest' env session' key' gen = Request gets'' cookies' env langs' nonce
  where
    gets' = queryToQueryText $ W.queryString env
    gets'' = map (second $ fromMaybe "") gets'
    reqCookie = lookup "Cookie" $ W.requestHeaders env
    cookies' = maybe [] parseCookiesText reqCookie
    acceptLang = lookup "Accept-Language" $ W.requestHeaders env
    langs = map (pack . S8.unpack) $ maybe [] NWP.parseHttpAccept acceptLang
    -- The language preferences are prioritized as follows:
    langs' = catMaybes [ join $ lookup langKey gets' -- Query _LANG
                       , lookup langKey cookies'     -- Cookie _LANG
                       , lookup langKey session'     -- Session _LANG
                       ] ++ langs                    -- Accept-Language(s)
    -- If sessions are disabled nonces should not be used (any
    -- nonceKey present in the session is ignored). If sessions
    -- are enabled and a session has no nonceKey a new one is
    -- generated.
    nonce = case (key', lookup nonceKey session') of
                (Nothing, _) -> Nothing
                (_, Just x)  -> Just x
                _            -> Just $ pack $ randomString 10 gen

-- | Generate a random String of alphanumerical characters
-- (a-z, A-Z, and 0-9) of the given length using the given
-- random number generator.
randomString :: RandomGen g => Int -> g -> String
randomString len = take len . map toChar . randomRs (0, 61)
  where
    toChar i
        | i < 26 = toEnum $ i + fromEnum 'A'
        | i < 52 = toEnum $ i + fromEnum 'a' - 26
        | otherwise = toEnum $ i + fromEnum '0' - 52

-- | A tuple containing both the POST parameters and submitted files.
type RequestBodyContents =
    ( [(Text, Text)]
    , [(Text, FileInfo)]
    )

data FileInfo = FileInfo
    { fileName :: Text
    , fileContentType :: Text
    , fileContent :: L.ByteString
    }
    deriving (Eq, Show)
