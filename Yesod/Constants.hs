---------------------------------------------------------
--
-- Module        : Yesod.Constants
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
-- Constants used throughout Yesod.
--
---------------------------------------------------------
module Yesod.Constants
    ( authCookieName
    , authDisplayName
    , encryptedCookies
    ) where

authCookieName :: String
authCookieName = "IDENTIFIER"

authDisplayName :: String
authDisplayName = "DISPLAY_NAME"

encryptedCookies :: [String]
encryptedCookies = [authDisplayName, authCookieName]
