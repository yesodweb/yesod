{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
---------------------------------------------------------
--
-- Module        : Web.Restful.I18N
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
-- Simple method for internationalization.
--
---------------------------------------------------------
module Web.Restful.I18N
    ( Language
    , Translator
    , I18N (..)
    , toTranslator
    ) where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import Data.ByteString.Class

type Language = String
type Translator = [Language] -> B.ByteString

class I18N a where
    translate :: a -> Translator

instance I18NString a => I18N a where
    translate a langs = toLazyByteString $ helper langs where
        helper [] = defTrans a
        helper (l:ls) =
            case tryTranslate a l of
                Nothing -> helper ls
                Just s -> s

class I18NString a where
    defTrans :: a -> String
    tryTranslate :: a -> Language -> Maybe String

toTranslator :: LazyByteString lbs => lbs -> Translator
toTranslator = translate . toLazyByteString

instance I18N B.ByteString where
    translate = const

instance I18N BS.ByteString where
    translate bs _ = toLazyByteString bs

instance I18NString String where
    defTrans = id
    tryTranslate = const . Just
