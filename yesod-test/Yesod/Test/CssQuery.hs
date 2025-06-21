{-# LANGUAGE OverloadedStrings #-}

-- | Parsing CSS selectors into queries.
module Yesod.Test.CssQuery
    ( SelectorGroup (..)
    , SelectorType (..)
    , Selector (..)
    , ANPlusB (..)
    , PseudoSelector (..)
    , parseQuery
    ) where

import Prelude hiding (takeWhile)
import Data.Text (Text)
import Data.Attoparsec.Text
import Control.Applicative (many, (<|>))
import Data.Char

import qualified Data.Text as T

data SelectorGroup
  = DirectChildren [SelectorType]
  | DeepChildren [SelectorType]
  deriving (Show, Eq)

-- | The notation used in CSS selectors level 3 to denote a rule
-- for matching indexed occurrences of elements.
--
-- Examples: odd, 3n+4, 2.
data ANPlusB
  = Repetition Int Int
  | Position Int
  | Odd
  | Even
  deriving (Show, Eq)

data PseudoSelector
  = FirstChild
  | LastChild
  | NthChild ANPlusB 
  deriving (Show, Eq)

data SelectorType
  = CompoundSelector Selector [PseudoSelector]
  | SimpleSelector Selector
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
  | Asterisk
  deriving (Show, Eq)


-- The official syntax specification for CSS2 can be found here:
--      http://www.w3.org/TR/CSS2/syndata.html
-- but that spec is tricky to fully support. Instead we do the minimal and we
-- can extend it as needed.


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
cssQuery = many (char ' ') >> sepBy rules (char ',' >> many (char ' '))

rules :: Parser [SelectorGroup]
rules = many $ directChildren <|> deepChildren

directChildren :: Parser SelectorGroup
directChildren =
    string "> " >> many (char ' ') >> DirectChildren <$> pOptionalTrailingSpace parseSelectorTypes

deepChildren :: Parser SelectorGroup
deepChildren = pOptionalTrailingSpace $ DeepChildren <$> parseSelectorTypes

parseSelectorTypes :: Parser [SelectorType]
parseSelectorTypes = many1 parseSelectorType

parseSelectorType :: Parser SelectorType 
parseSelectorType = choice
  [ CompoundSelector
      <$> option Asterisk parseSelector 
      <*> many1 parsePseudoSelector
  , SimpleSelector <$> parseSelector
  ]

parseSelector :: Parser Selector
parseSelector = choice [parseId, parseClass, parseTag, parseAttr, parseAsterisk]

parseAsterisk :: Parser Selector
parseAsterisk = char '*' >> pure Asterisk

parseId :: Parser Selector
parseId = char '#' >> ById <$> pIdent

parseClass :: Parser Selector
parseClass = char '.' >> ByClass <$> pIdent

parseTag :: Parser Selector
parseTag = ByTagName <$> pIdent

parseAttr :: Parser Selector
parseAttr = pSquare $ choice
    [ ByAttrEquals <$> pIdent <*> (string "=" *> pAttrValue)
    , ByAttrContains <$> pIdent <*> (string "*=" *> pAttrValue)
    , ByAttrStarts <$> pIdent <*> (string "^=" *> pAttrValue)
    , ByAttrEnds <$> pIdent <*> (string "$=" *> pAttrValue)
    , ByAttrExists <$> pIdent
    ]

spaceSurrounded :: Parser a -> Parser a
spaceSurrounded m = skipSpaces *> m <* skipSpaces
  where skipSpaces = skipMany $ char ' '

pArg :: Parser ANPlusB
pArg = char '(' *> spaceSurrounded pANPlusB <* char ')'

pANPlusB :: Parser ANPlusB 
pANPlusB = choice [repetition, position, fixed]
  where repetition = Repetition
          <$> option 1
            ( choice 
                [ char '-' *> fmap negate decimal
                , char '-' >> pure (-1)
                , decimal
                ]
            ) <* char 'n'
          <*> option 0 
            ( choice 
                [ spaceSurrounded (char '+') *> decimal
                , spaceSurrounded (char '-') *> fmap negate decimal
                ]
            )
        position = Position <$> decimal
        fixed = choice
          [ string "odd" >> pure Odd
          , string "even" >> pure Even
          ]

parsePseudoSelector :: Parser PseudoSelector 
parsePseudoSelector = char ':' >> choice
  [ string "first-child" >> pure FirstChild
  , string "last-child" >> pure LastChild
  , string "nth-child" >> NthChild <$> pArg
  ] <|> fail "unknown or unsupported pseudo-class"

-- | pIdent : Parse an identifier (not yet supporting escapes and unicode as
-- part of the identifier). Basically the regex: [-]?[_a-zA-Z][_a-zA-Z0-9]*
pIdent :: Parser Text
pIdent = do
    leadingMinus <- string "-" <|> pure ""
    nmstart <- T.singleton <$> satisfy (\c -> isAlpha c || c == '_')
    nmchar <- takeWhile (\c -> isAlphaNum c || c == '_' || c == '-')
    return $ T.concat [ leadingMinus, nmstart, nmchar ]


pAttrValue :: Parser Text
pAttrValue = takeWhile (/= ']')

pSquare :: Parser a -> Parser a
pSquare p = char '[' *> p <* char ']'

pOptionalTrailingSpace :: Parser a -> Parser a
pOptionalTrailingSpace p = p <* many (char ' ')
