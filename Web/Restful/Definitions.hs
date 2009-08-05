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
    , ParsedResource (..)
    , ResourceParser
    , HasResourceParser (..)
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

data ParsedResource a = ParsedResource
    { resourceName :: a
    , urlParameters :: [(String, String)]
    }

type ResourceParser a = Resource -> Maybe (ParsedResource a)

class HasResourceParser a where
    resourceParser :: ResourceParser a
    simpleParse :: a -> Maybe (ParsedResource a)
    simpleParse x = Just $ ParsedResource x []
