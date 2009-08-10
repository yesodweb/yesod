{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
---------------------------------------------------------
--
-- Module        : Web.Restful.Definitions
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
module Web.Restful.Definitions
    ( Verb (..)
    , toVerb
    , Resource
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
