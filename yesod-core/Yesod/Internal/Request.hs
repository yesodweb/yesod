{-# LANGUAGE OverloadedStrings #-}
module Yesod.Internal.Request
    ( parseWaiRequest
    , Request (..)
    , RequestBodyContents
    , FileInfo (..)
    ) where

import Control.Applicative ((<$>))
import Control.Arrow (second)
import qualified Network.Wai.Parse as NWP
import Yesod.Internal
import qualified Network.Wai as W
import System.Random (randomRs, newStdGen)
import Web.Cookie (parseCookiesText)
import Data.Monoid (mempty)
import qualified Data.ByteString.Char8 as S8
import Data.Text (Text, pack)
import Network.HTTP.Types (queryToQueryText)
import Control.Monad (join)
import Data.Maybe (fromMaybe)
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
parseWaiRequest env session' key' = do
    let gets' = queryToQueryText $ W.queryString env
    let reqCookie = fromMaybe mempty $ lookup "Cookie"
                  $ W.requestHeaders env
        cookies' = parseCookiesText reqCookie
        acceptLang = lookup "Accept-Language" $ W.requestHeaders env
        langs = map (pack . S8.unpack) $ maybe [] NWP.parseHttpAccept acceptLang
        langs' = case lookup langKey session' of
                    Nothing -> langs
                    Just x -> x : langs
        langs'' = case lookup langKey cookies' of
                    Nothing -> langs'
                    Just x -> x : langs'
        langs''' = case join $ lookup langKey gets' of
                     Nothing -> langs''
                     Just x -> x : langs''
    nonce <- case (key', lookup nonceKey session') of
                (Nothing, _) -> return Nothing
                (_, Just x) -> return $ Just x
                _ -> Just . pack . randomString 10 <$> newStdGen
    let gets'' = map (second $ fromMaybe "") gets'
    return $ Request gets'' cookies' env langs''' nonce
  where
    randomString len = map toChar . take len . randomRs (0, 61)
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
