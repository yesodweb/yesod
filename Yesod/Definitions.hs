{-# LANGUAGE MultiParamTypeClasses #-}
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
    ( Verb (..)
    , Resource
    , Approot (..)
    , Language
    ) where

import qualified Hack
import Data.Convertible.Text

data Verb = Get | Put | Delete | Post
    deriving (Eq, Show)

instance ConvertSuccess Hack.RequestMethod Verb where
    convertSuccess Hack.PUT = Put
    convertSuccess Hack.DELETE = Delete
    convertSuccess Hack.POST = Post
    convertSuccess _ = Get

type Resource = [String]

-- | An absolute URL to the base of this application. This can almost be done
-- programatically, but due to ambiguities in different ways of doing URL
-- rewriting for (fast)cgi applications, it should be supplied by the user.
newtype Approot = Approot { unApproot :: String }

type Language = String
