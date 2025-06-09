{-# LANGUAGE OverloadedStrings #-}

-- | This module exposes functions that are used internally by yesod-test.
-- The functions exposed here are _not_ a stable API—they may be changed or removed without any major version bump.
--
-- That said, you may find them useful if your application can accept API breakage.
module Yesod.Test.Internal
  ( getBodyTextPreview
  , contentTypeHeaderIsUtf8
  , assumedUTF8ContentTypes
  ) where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as DTLE
import qualified Yesod.Core.Content as Content

-- | Helper function to get the first 1024 characters of the body, assuming it is UTF-8.
-- This function is used to preview the body in case of an assertion failure.
--
-- @since 1.6.10
getBodyTextPreview :: LBS.ByteString -> T.Text
getBodyTextPreview body =
  let characterLimit = 1024
      textBody = TL.toStrict $ DTLE.decodeUtf8 body
  in if T.length textBody < characterLimit
        then textBody
        else T.take characterLimit textBody <> "... (use `printBody` to see complete response body)"

-- | Helper function to determine if we can print a body as plain text, for debugging purposes.
--
-- @since 1.6.10
contentTypeHeaderIsUtf8 :: BS8.ByteString -> Bool
contentTypeHeaderIsUtf8 contentTypeBS =
      -- Convert to Text, so we can use T.splitOn
  let contentTypeText = T.toLower $ TE.decodeUtf8 contentTypeBS
      isUTF8FromCharset = case T.splitOn "charset=" contentTypeText of
        -- Either a specific designation as UTF-8, or ASCII (which is a subset of UTF-8)
        [_, charSet] -> any (`T.isInfixOf` charSet) ["utf-8", "us-ascii"]
        _ -> False

      isInferredUTF8FromContentType = BS8.takeWhile (/= ';') contentTypeBS `Set.member` assumedUTF8ContentTypes

  in isUTF8FromCharset || isInferredUTF8FromContentType

-- | List of Content-Types that are assumed to be UTF-8 (e.g. JSON).
--
-- @since 1.6.10
assumedUTF8ContentTypes :: Set.Set BS8.ByteString
assumedUTF8ContentTypes = Set.fromList $ map Content.simpleContentType
  [ Content.typeHtml
  , Content.typePlain
  , Content.typeJson
  , Content.typeXml
  , Content.typeAtom
  , Content.typeRss
  , Content.typeSvg
  , Content.typeJavascript
  , Content.typeCss
  ]
