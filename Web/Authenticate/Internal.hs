{-# LANGUAGE DeriveDataTypeable #-}
module Web.Authenticate.Internal
    ( qsEncode
    , qsUrl
    , AuthenticateException (..)
    ) where

import Codec.Binary.UTF8.String (encode)
import Numeric (showHex)
import Data.List (intercalate)
import Data.Typeable (Typeable)
import Control.Exception (Exception)

data AuthenticateException =
      RpxnowException String
    | NormalizationException String
    | DiscoveryException String
    | AuthenticationException String
  deriving (Show, Typeable)
instance Exception AuthenticateException

qsUrl :: String -> [(String, String)] -> String -- FIXME remove
qsUrl s [] = s
qsUrl url pairs =
    url ++ delim : intercalate "&" (map qsPair pairs)
  where
    qsPair (x, y) = qsEncode x ++ '=' : qsEncode y
    delim = if '?' `elem` url then '&' else '?'

qsEncode :: String -> String
qsEncode =
    concatMap go . encode
  where
    go 32 = "+" -- space
    go 46 = "."
    go 45 = "-"
    go 126 = "~"
    go 95 = "_"
    go c
        | 48 <= c && c <= 57 = [w2c c]
        | 65 <= c && c <= 90 = [w2c c]
        | 97 <= c && c <= 122 = [w2c c]
    go c = '%' : showHex c ""
    w2c = toEnum . fromEnum
