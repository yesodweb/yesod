{-# LANGUAGE FlexibleContexts #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Network.OpenID.Normalization
-- Copyright   : (c) Trevor Elliott, 2008
-- License     : BSD3
--
-- Maintainer  : Trevor Elliott <trevor@geekgateway.com>
-- Stability   : 
-- Portability : 
--

module OpenId2.Normalization
    ( normalize
    ) where

-- Friends
import OpenId2.Types

-- Libraries
import Control.Applicative
import Control.Monad
import Data.List
import Network.URI
    ( uriToString, normalizeCase, normalizeEscape
    , normalizePathSegments, parseURI, uriPath, uriScheme, uriFragment
    )
import Data.Text (Text, pack, unpack)
import Control.Monad.IO.Class
import Control.Exception (throwIO)

normalize :: MonadIO m => Text -> m Identifier
normalize ident =
    case normalizeIdentifier $ Identifier ident of
        Just i -> return i
        Nothing -> liftIO $ throwIO $ NormalizationException $ unpack ident

-- | Normalize an identifier, discarding XRIs.
normalizeIdentifier :: Identifier -> Maybe Identifier
normalizeIdentifier  = normalizeIdentifier' (const Nothing)


-- | Normalize the user supplied identifier, using a supplied function to
-- normalize an XRI.
normalizeIdentifier' :: (String -> Maybe String) -> Identifier
                     -> Maybe Identifier
normalizeIdentifier' xri (Identifier str')
  | null str                  = Nothing
  | "xri://" `isPrefixOf` str = (Identifier . pack) `fmap` xri str
  | head str `elem` "=@+$!"   = (Identifier . pack) `fmap` xri str
  | otherwise = fmt `fmap` (url >>= norm)
  where
    str = unpack str'
    url = parseURI str <|> parseURI ("http://" ++ str)

    norm uri = validScheme >> return u
      where
        scheme'     = uriScheme uri
        validScheme = guard (scheme' == "http:" || scheme' == "https:")
        u = uri { uriFragment = "", uriPath = path' }
        path' | null (uriPath uri) = "/"
              | otherwise          = uriPath uri

    fmt u = Identifier
          $ pack
          $ normalizePathSegments
          $ normalizeEscape
          $ normalizeCase
          $ uriToString (const "") u []
