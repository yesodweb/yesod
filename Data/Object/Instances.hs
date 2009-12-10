{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
    ) where

import Data.Object.Text
import qualified Data.ByteString.Lazy as B
import Web.Encodings (encodeJson)
import Text.Yaml (encodeText')
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy (Text)
import Data.Convertible.Text

newtype Json = Json { unJson :: Text }
instance ConvertAttempt (Object Text Text) Json where
    convertAttempt = return . convertSuccess
instance ConvertSuccess (Object Text Text) Json where
    convertSuccess = Json . helper where
        helper :: TextObject -> Text
        helper (Scalar s) = LT.concat
            [ LT.pack "\""
            , bsToText $ encodeJson $ convertSuccess s
            , LT.pack "\""
            ]
        helper (Sequence s) = LT.concat
            [ LT.pack "["
            , LT.intercalate (LT.pack ",") $ map helper s
            , LT.pack "]"
            ]
        helper (Mapping m) = LT.concat
            [ LT.pack "{"
            , LT.intercalate (LT.pack ",") $ map helper2 m
            , LT.pack "}"
            ]
        helper2 :: (Text, TextObject) -> Text
        helper2 (k, v) = LT.concat
            [ LT.pack "\""
            , bsToText $ encodeJson $ convertSuccess k
            , LT.pack "\":"
            , helper v
            ]

bsToText :: B.ByteString -> Text
bsToText = convertSuccess

newtype Yaml = Yaml { unYaml :: Text }
instance ConvertAttempt (Object Text Text) Yaml where
    convertAttempt = return . convertSuccess
instance ConvertSuccess (Object Text Text) Yaml where
    convertSuccess = Yaml . convertSuccess . encodeText'

-- | Represents as an entire HTML 5 document by using the following:
--
-- * A scalar is a paragraph.
-- * A sequence is an unordered list.
-- * A mapping is a definition list.
newtype Html = Html { unHtml :: Text }

instance ConvertAttempt (Object Text Text) Html where
    convertAttempt = return . convertSuccess
instance ConvertSuccess (Object Text Text) Html where
    convertSuccess o = Html $ LT.concat
        [ LT.pack "<!DOCTYPE html>\n<html><body>" -- FIXME full doc or just fragment?
        , helper o
        , LT.pack "</body></html>"
        ] where
            helper :: TextObject -> Text
            helper (Scalar s) = LT.concat
                [ LT.pack "<p>"
                , s
                , LT.pack "</p>"
                ]
            helper (Sequence []) = LT.pack "<ul></ul>"
            helper (Sequence s) = LT.concat
                [ LT.pack "<ul><li>"
                , LT.intercalate (LT.pack "</li><li>") $ map helper s
                , LT.pack "</li></ul>"
                ]
            helper (Mapping m) = LT.concat $
                LT.pack "<dl>" :
                map helper2 m ++
                [ LT.pack "</dl>" ]
            helper2 :: (Text, TextObject) -> Text
            helper2 (k, v) = LT.concat
                [ LT.pack "<dt>"
                , k
                , LT.pack "</dt><dd>"
                , helper v
                , LT.pack "</dd>"
                ]
