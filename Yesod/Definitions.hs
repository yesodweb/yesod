{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
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
    , toVerb
    , Resource
    , Approot (..)
    ) where

import qualified Hack

data Verb = Get | Put | Delete | Post
    deriving (Eq, Show)

toVerb :: Hack.RequestMethod -> Verb
toVerb Hack.PUT = Put
toVerb Hack.DELETE = Delete
toVerb Hack.POST = Post
toVerb _ = Get

type Resource = [String]

-- | An absolute URL to the base of this application. This can almost be done
-- programatically, but due to ambiguities in different ways of doing URL
-- rewriting for (fast)cgi applications, it should be supplied by the user.
newtype Approot = Approot { unApproot :: String }
