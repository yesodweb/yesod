{-# LANGUAGE OverloadedStrings #-}
-- | Parsing CSS selectors into queries.
module Yesod.Test.CssQuery
    ( SelectorGroup (..)
    , Selector (..)
    , parseQuery
    ) where

import Prelude hiding (takeWhile)
import Data.Text (Text)
import Data.Attoparsec.Text
import Control.Applicative (many, (<|>), optional)

data SelectorGroup
  = DirectChildren [Selector]
  | DeepChildren [Selector]
  deriving (Show, Eq)

data Selector
  = ById Text
  | ByClass Text
  | ByTagName Text
  | ByAttrExists Text
  | ByAttrEquals Text Text
  | ByAttrContains Text Text
  | ByAttrStarts Text Text
  | ByAttrEnds Text Text
  deriving (Show, Eq)

-- | Parses a query into an intermediate format which is easy to feed to HXT
--
-- * The top-level lists represent the top level comma separated queries.
--
-- * SelectorGroup is a group of qualifiers which are separated
--   with spaces or > like these three: /table.main.odd tr.even > td.big/
--
-- * A SelectorGroup as a list of Selector items, following the above example
--   the selectors in the group are: /table/, /.main/ and /.odd/
parseQuery :: Text -> Either String [[SelectorGroup]]
parseQuery = parseOnly cssQuery

-- Below this line is the Parsec parser for css queries.
cssQuery :: Parser [[SelectorGroup]]
cssQuery = sepBy rules (char ',' >> (optional (char ' ')))

rules :: Parser [SelectorGroup]
rules = many $ directChildren <|> deepChildren

directChildren :: Parser SelectorGroup
directChildren = do
  _ <- char '>'
  _ <- char ' '
  sels <- selectors
  _ <- optional $ char ' '
  return $ DirectChildren sels

deepChildren :: Parser SelectorGroup
deepChildren = do 
  sels <- selectors
  _ <- optional $ char ' '
  return $ DeepChildren sels
  
selectors :: Parser [Selector]
selectors = many1 $ parseId
  <|> parseClass
  <|> parseTag
  <|> parseAttr

parseId :: Parser Selector
parseId = do
  _ <- char '#'
  x <- takeWhile $ flip notElem ",#.[ >"
  return $ ById x

parseClass :: Parser Selector
parseClass = do
  _ <- char '.'
  x <- takeWhile $ flip notElem ",#.[ >"
  return $ ByClass x

parseTag :: Parser Selector
parseTag = do
  x <- takeWhile1 $ flip notElem ",#.[ >"
  return $ ByTagName x

parseAttr :: Parser Selector
parseAttr = do
  _ <- char '['
  name <- takeWhile $ flip notElem ",#.=$^*]"
  (parseAttrExists name)
    <|> (parseAttrWith "=" ByAttrEquals name)
    <|> (parseAttrWith "*=" ByAttrContains name)
    <|> (parseAttrWith "^=" ByAttrStarts name)
    <|> (parseAttrWith "$=" ByAttrEnds name)

parseAttrExists :: Text -> Parser Selector
parseAttrExists attrname = do
  _ <- char ']'
  return $ ByAttrExists attrname

parseAttrWith :: Text -> (Text -> Text -> Selector) -> Text -> Parser Selector
parseAttrWith sign constructor name = do
  _ <- string sign
  value <- takeWhile $ flip notElem ",#.]"
  _ <- char ']'
  return $ constructor name value
