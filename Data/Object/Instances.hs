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
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import Data.ByteString.Class
import Web.Encodings (encodeJson)
import Text.Yaml (encode)

class SafeFromObject a where
    safeFromObject :: Object -> a

newtype Json = Json { unJson :: B.ByteString }
instance SafeFromObject Json where
    safeFromObject = Json . helper where
        helper :: Object -> B.ByteString
        helper (Scalar s) = B.concat
            [ toLazyByteString "\""
            , encodeJson $ fromStrictByteString s
            , toLazyByteString "\""
            ]
        helper (Sequence s) = B.concat
            [ toLazyByteString "["
            , B.intercalate (toLazyByteString ",") $ map helper s
            , toLazyByteString "]"
            ]
        helper (Mapping m) = B.concat
            [ toLazyByteString "{"
            , B.intercalate (toLazyByteString ",") $ map helper2 m
            , toLazyByteString "}"
            ]
        helper2 :: (BS.ByteString, Object) -> B.ByteString
        helper2 (k, v) = B.concat
            [ toLazyByteString "\""
            , encodeJson $ fromStrictByteString k
            , toLazyByteString "\":"
            , helper v
            ]

newtype Yaml = Yaml { unYaml :: B.ByteString }
instance SafeFromObject Yaml where
    safeFromObject = Yaml . encode

-- | Represents as an entire HTML 5 document by using the following:
--
-- * A scalar is a paragraph.
-- * A sequence is an unordered list.
-- * A mapping is a definition list.
newtype Html = Html { unHtml :: B.ByteString }

instance SafeFromObject Html where
    safeFromObject o = Html $ B.concat
        [ toLazyByteString "<!DOCTYPE html>\n<html><body>" -- FIXME full doc or just fragment?
        , helper o
        , toLazyByteString "</body></html>"
        ] where
            helper :: Object -> B.ByteString
            helper (Scalar s) = B.concat
                [ toLazyByteString "<p>"
                , toLazyByteString s
                , toLazyByteString "</p>"
                ]
            helper (Sequence []) = toLazyByteString "<ul></ul>"
            helper (Sequence s) = B.concat
                [ toLazyByteString "<ul><li>"
                , B.intercalate (toLazyByteString "</li><li>") $ map helper s
                , toLazyByteString "</li></ul>"
                ]
            helper (Mapping m) = B.concat $
                toLazyByteString "<dl>" :
                map helper2 m ++
                [ toLazyByteString "</dl>" ]
            helper2 :: (BS.ByteString, Object) -> B.ByteString
            helper2 (k, v) = B.concat $
                [ toLazyByteString "<dt>"
                , toLazyByteString k
                , toLazyByteString "</dt><dd>"
                , helper v
                , toLazyByteString "</dd>"
                ]
