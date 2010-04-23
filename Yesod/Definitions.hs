{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
---------------------------------------------------------
--
-- Module        : Yesod.Definitions
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
-- Definitions throughout Restful.
--
---------------------------------------------------------
module Yesod.Definitions
    ( -- * Type synonyms
      Approot
    , Language
      -- * Constant values
    , authCookieName
    , authDisplayName
    , encryptedCookies
    , langKey
    , destCookieName
    , destCookieTimeout
      -- * Other
    , Routes
    ) where

import Data.ByteString.Char8 (pack, ByteString)
import Web.Routes.Quasi (Routes)

-- | An absolute URL to the base of this application. This can almost be done
-- programatically, but due to ambiguities in different ways of doing URL
-- rewriting for (fast)cgi applications, it should be supplied by the user.
type Approot = String

type Language = String

authCookieName :: String
authCookieName = "IDENTIFIER"

authDisplayName :: String
authDisplayName = "DISPLAY_NAME"

encryptedCookies :: [ByteString] -- FIXME make this extensible
encryptedCookies = [pack authDisplayName, pack authCookieName]

langKey :: String
langKey = "_LANG"

destCookieName :: String
destCookieName = "DEST"

destCookieTimeout :: Int
destCookieTimeout = 120
