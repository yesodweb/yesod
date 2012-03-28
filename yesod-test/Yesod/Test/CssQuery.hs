-- | Parsing CSS selectors into queries.
module Yesod.Test.CssQuery
    ( SelectorGroup (..)
    , Selector (..)
    , parseQuery
    ) where

import Text.ParserCombinators.Parsec
import Text.Parsec.Prim (Parsec)

data SelectorGroup
  = DirectChildren [Selector]
  | DeepChildren [Selector]
  deriving (Show, Eq)

data Selector
  = ById String
  | ByClass String
  | ByTagName String
  | ByAttrExists String
  | ByAttrEquals String String
  | ByAttrContains String String
  | ByAttrStarts String String
  | ByAttrEnds String String
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
parseQuery :: String -> Either ParseError [[SelectorGroup]]
parseQuery = parse cssQuery "" 

-- Below this line is the Parsec parser for css queries.
cssQuery :: Parsec String u [[SelectorGroup]]
cssQuery = sepBy rules (char ',' >> (optional (char ' ')))

rules :: Parsec String u [SelectorGroup]
rules = many $ directChildren <|> deepChildren

directChildren :: Parsec String u SelectorGroup
directChildren = do
  _ <- char '>'
  _ <- char ' '
  sels <- selectors
  optional $ char ' '
  return $ DirectChildren sels

deepChildren :: Parsec String u SelectorGroup
deepChildren = do 
  sels <- selectors
  optional $ char ' '
  return $ DeepChildren sels
  
selectors :: Parsec String u [Selector]
selectors = many1 $ parseId
  <|> parseClass
  <|> parseTag
  <|> parseAttr

parseId :: Parsec String u Selector
parseId = do
  _ <- char '#'
  x <- many $ noneOf ",#.[ >"
  return $ ById x

parseClass :: Parsec String u Selector
parseClass = do
  _ <- char '.'
  x <- many $ noneOf ",#.[ >"
  return $ ByClass x

parseTag :: Parsec String u Selector
parseTag = do
  x <- many1 $ noneOf ",#.[ >"
  return $ ByTagName x

parseAttr :: Parsec String u Selector
parseAttr = do
  _ <- char '['
  name <- many $ noneOf ",#.=$^*]"
  (parseAttrExists name)
    <|> (parseAttrWith "=" ByAttrEquals name)
    <|> (parseAttrWith "*=" ByAttrContains name)
    <|> (parseAttrWith "^=" ByAttrStarts name)
    <|> (parseAttrWith "$=" ByAttrEnds name)

parseAttrExists :: String -> Parsec String u Selector
parseAttrExists attrname = do
  _ <- char ']'
  return $ ByAttrExists attrname

parseAttrWith :: String -> (String -> String -> Selector) -> String -> Parsec String u Selector
parseAttrWith sign constructor name = do
  _ <- string sign
  value <- many $ noneOf ",#.]"
  _ <- char ']'
  return $ constructor name value
