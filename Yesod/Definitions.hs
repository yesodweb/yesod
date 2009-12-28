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
    ( Verb (..)
    , Resource
    , Approot (..)
    , Language
    , Location (..)
    , showLocation
    ) where

import qualified Hack
import Data.Convertible.Text
import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Language.Haskell.TH.Syntax

data Verb = Get | Put | Delete | Post
    deriving (Eq, Show, Enum, Bounded)
instance Lift Verb where
    lift = return . ConE . mkName . show
instance ConvertAttempt String Verb where
    convertAttempt "Get" = return Get
    convertAttempt "Put" = return Put
    convertAttempt "Delete" = return Delete
    convertAttempt "Post" = return Post
    convertAttempt s = failure $ InvalidVerb s
newtype InvalidVerb = InvalidVerb String
    deriving (Show, Typeable)
instance Exception InvalidVerb

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

-- | A location string. Can either be given absolutely or as a suffix for the
-- 'Approot'.
data Location = AbsLoc String | RelLoc String

-- | Display a 'Location' in absolute form.
showLocation :: Approot -> Location -> String
showLocation _ (AbsLoc s) = s
showLocation (Approot ar) (RelLoc s) = ar ++ s
