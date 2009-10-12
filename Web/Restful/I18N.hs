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
    , translateBS
    , NoI18N (..)
    ) where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import Data.ByteString.Class

type Language = String
type Translator = [Language] -> B.ByteString

class I18N a where
    translate :: a -> Translator
    translate a langs = toLazyByteString $ helper langs where
        helper [] = defTrans a
        helper (l:ls) =
            case tryTranslate a l of
                Nothing -> helper ls
                Just s -> s

    defTrans :: a -> String
    tryTranslate :: a -> Language -> Maybe String

instance I18N String where
    defTrans = id
    tryTranslate = const . Just

translateBS :: I18N a => a -> Translator
translateBS a = toLazyByteString . translate a

class NoI18N a where
    noTranslate :: a -> Translator

instance NoI18N B.ByteString where
    noTranslate = const

instance NoI18N BS.ByteString where
    noTranslate = const . toLazyByteString

instance NoI18N String where
    noTranslate = const . toLazyByteString
