{-# LANGUAGE OverloadedStrings #-}
-- | Parse an HTML document into xml-conduit's Document.
--
-- Assumes UTF-8 encoding.
module Yesod.Test.HtmlParse
    ( parseHtml
    ) where

import Text.HTML.TagStream
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Text.XML
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Functor.Identity (runIdentity)
import Control.Monad.Trans.Resource (runExceptionT)
import Data.XML.Types (Event (..), Content (ContentText))
import Control.Arrow ((***))
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Set as Set

parseHtml :: L.ByteString -> Either String Document
parseHtml lbs =
      either (Left . show) Right
    $ runIdentity
    $ runExceptionT
    $ CL.sourceList (L.toChunks lbs)
   $$ tokenStream =$ (CL.concatMap toEvent =$ fromEvents)

toEvent :: Token -> [Event]
toEvent (TagOpen bsname bsattrs isClose') =
    EventBeginElement name attrs : if isClose then [EventEndElement name] else []
  where
    name = toName bsname
    attrs = map (toName *** (return . ContentText . decodeUtf8With lenientDecode)) bsattrs
    isClose = isClose' || isVoid bsname
toEvent (TagClose bsname) = [EventEndElement $ toName bsname]
toEvent (Text bs) = [EventContent $ ContentText $ decodeUtf8With lenientDecode bs]
toEvent (Comment bs) = [EventComment $ decodeUtf8With lenientDecode bs]
toEvent Special{} = []
toEvent Incomplete{} = []

toName :: S.ByteString -> Name
toName bs = Name (decodeUtf8With lenientDecode bs) Nothing Nothing

isVoid :: S.ByteString -> Bool
isVoid = flip Set.member $ Set.fromList
    [ "area"
    , "base"
    , "br"
    , "col"
    , "command"
    , "embed"
    , "hr"
    , "img"
    , "input"
    , "keygen"
    , "link"
    , "meta"
    , "param"
    , "source"
    , "track"
    , "wbr"
    ]
