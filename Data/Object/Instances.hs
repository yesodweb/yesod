{-# LANGUAGE FlexibleInstances #-}
---------------------------------------------------------
--
-- Module        : Data.Object.Instances
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
-- Instances for converting various types of data into Data.Object.Object.
--
---------------------------------------------------------
module Data.Object.Instances
    ( Json (..)
    , Yaml (..)
    , Html (..)
    , SafeFromObject (..)
    ) where

import Data.Object
import qualified Data.ByteString as B
import Data.ByteString.Class
import Web.Encodings (encodeJson)
import qualified Text.Yaml as Y

class SafeFromObject a where
    safeFromObject :: Object -> a

newtype Json = Json { unJson :: B.ByteString }
instance SafeFromObject Json where
    safeFromObject = Json . helper where
        helper :: Object -> B.ByteString
        helper (Scalar s) = B.concat
            [ toStrictByteString "\""
            , encodeJson $ fromStrictByteString s
            , toStrictByteString "\""
            ]
        helper (Sequence s) = B.concat
            [ toStrictByteString "["
            , B.intercalate (toStrictByteString ",") $ map helper s
            , toStrictByteString "]"
            ]
        helper (Mapping m) = B.concat
            [ toStrictByteString "{"
            , B.intercalate (toStrictByteString ",") $ map helper2 m
            , toStrictByteString "}"
            ]
        helper2 :: (B.ByteString, Object) -> B.ByteString
        helper2 (k, v) = B.concat
            [ toStrictByteString "\""
            , encodeJson $ fromStrictByteString k
            , toStrictByteString "\":"
            , helper v
            ]

newtype Yaml = Yaml { unYaml :: B.ByteString }
instance SafeFromObject Yaml where
    safeFromObject = Yaml . Y.encode

-- | Represents as an entire HTML 5 document by using the following:
--
-- * A scalar is a paragraph.
-- * A sequence is an unordered list.
-- * A mapping is a definition list.
newtype Html = Html { unHtml :: B.ByteString }

instance SafeFromObject Html where
    safeFromObject o = Html $ B.concat
        [ toStrictByteString "<!DOCTYPE html>\n<html><body>"
        , helper o
        , toStrictByteString "</body></html>"
        ] where
            helper :: Object -> B.ByteString
            helper (Scalar s) = B.concat
                [ toStrictByteString "<p>"
                , s
                , toStrictByteString "</p>"
                ]
            helper (Sequence []) = toStrictByteString "<ul></ul>"
            helper (Sequence s) = B.concat
                [ toStrictByteString "<ul><li>"
                , B.intercalate (toStrictByteString "</li><li>") $ map helper s
                , toStrictByteString "</li></ul>"
                ]
            helper (Mapping m) = B.concat $
                toStrictByteString "<dl>" :
                map helper2 m ++
                [ toStrictByteString "</dl>" ]
            helper2 :: (B.ByteString, Object) -> B.ByteString
            helper2 (k, v) = B.concat $
                [ toStrictByteString "<dt>"
                , k
                , toStrictByteString "</dt><dd>"
                , helper v
                , toStrictByteString "</dd>"
                ]
