{-# LANGUAGE OverloadedStrings #-}
-- | Parse an HTML document into xml-conduit's Document.
--
-- Assumes UTF-8 encoding.
module Yesod.Test.HtmlParse
    ( parseHtml
    ) where

import qualified Data.ByteString.Lazy as L
import Text.XML (Document)
import qualified Text.HTML.DOM as HD

parseHtml :: L.ByteString -> Either String Document
parseHtml = Right . HD.parseLBS
