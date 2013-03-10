{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | Normal users should never need access to these.
--
-- Note that no guarantees of API stability are provided on this module. Use at your own risk.
module Yesod.Internal
    ( -- * Error responses
      ErrorResponse (..)
    , HandlerContents (..)
      -- * Header
    , Header (..)
      -- * Cookie names
    , langKey
      -- * Widgets
    , GWData (..)
    , Location (..)
    , UniqueList (..)
    , Script (..)
    , Stylesheet (..)
    , Title (..)
    , Head (..)
    , Body (..)
    , locationToHtmlUrl
    , runUniqueList
    , toUnique
      -- * Names
    , tokenKey
    ) where

import Text.Hamlet (HtmlUrl)
import Text.Blaze.Html (toHtml)
import Data.List (nub)

import Data.String (IsString)
import Yesod.Core.Types

langKey :: IsString a => a
langKey = "_LANG"

locationToHtmlUrl :: Location url -> HtmlUrl url
locationToHtmlUrl (Local url) render = toHtml $ render url []
locationToHtmlUrl (Remote s) _ = toHtml s

runUniqueList :: Eq x => UniqueList x -> [x]
runUniqueList (UniqueList x) = nub $ x []
toUnique :: x -> UniqueList x
toUnique = UniqueList . (:)

tokenKey :: IsString a => a
tokenKey = "_TOKEN"
