{-# LANGUAGE FlexibleInstances #-}
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
    , ParsedResource (..)
    , ResourceParser
    , ResourceName (..)
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

class Eq a => ResourceName a where
    toResourceName :: [String] -> a
instance ResourceName [String] where
    toResourceName = id

data ParsedResource a = ParsedResource
    { resourceName :: a
    , urlParameters :: [(String, String)]
    }

type ResourceParser a = Resource -> ParsedResource a
