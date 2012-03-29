{-# LANGUAGE OverloadedStrings #-}
{- |
This module uses HXT to transverse an HTML document using CSS selectors.

The most important function here is 'findBySelector', it takes a CSS query and
a string containing the HTML to look into,
and it returns a list of the HTML fragments that matched the given query.

Only a subset of the CSS spec is currently supported:

 * By tag name: /table td a/
 
 * By class names: /.container .content/

 * By Id: /#oneId/

 * By attribute: /[hasIt]/, /[exact=match]/, /[contains*=text]/, /[starts^=with]/, /[ends$=with]/
 
 * Union: /a, span, p/
 
 * Immediate children: /div > p/ 

 * Get jiggy with it: /div[data-attr=yeah] > .mon, .foo.bar div, #oneThing/

-}

module Yesod.Test.TransversingCSS (
  findBySelector,
  Html,
  Query,
  -- * For HXT hackers
  -- | These functions expose some low level details that you can blissfully ignore.
  parseQuery,
  runQuery,
  Selector(..),
  SelectorGroup(..)

  )
where

import Yesod.Test.CssQuery
import qualified Data.Text as T
import Yesod.Test.HtmlParse (parseHtml)
import Control.Applicative ((<$>), (<*>))
import Text.XML
import Text.XML.Cursor
import qualified Data.ByteString.Lazy as L
import Text.Blaze (toHtml)
import Text.Blaze.Renderer.String (renderHtml)
import Text.XML.Xml2Html ()

type Query = T.Text
type Html = L.ByteString

-- | Perform a css 'Query' on 'Html'. Returns Either
--
-- * Left: Query parse error.
--
-- * Right: List of matching Html fragments.
findBySelector :: Html -> Query -> Either String [String]
findBySelector html query = (\x -> map (renderHtml . toHtml . node) . runQuery x)
    <$> (fromDocument <$> parseHtml html)
    <*> parseQuery query

-- Run a compiled query on Html, returning a list of matching Html fragments.
runQuery :: Cursor -> [[SelectorGroup]] -> [Cursor]
runQuery html query = concatMap (runGroup html) query

runGroup :: Cursor -> [SelectorGroup] -> [Cursor]
runGroup c [] = [c]
runGroup c (DirectChildren s:gs) = concatMap (flip runGroup gs) $ c $/ selectors s
runGroup c (DeepChildren s:gs) = concatMap (flip runGroup gs) $ c $// selectors s

selectors :: [Selector] -> Cursor -> [Cursor]
selectors ss c
    | all (selector c) ss = [c]
    | otherwise = []

selector :: Cursor -> Selector -> Bool
selector c (ById x) = not $ null $ attributeIs "id" x c
selector c (ByClass x) =
    case attribute "class" c of
        t:_ -> x `elem` T.words t
        [] -> False
selector c (ByTagName t) = not $ null $ element (Name t Nothing Nothing) c
selector c (ByAttrExists t) = not $ null $ hasAttribute (Name t Nothing Nothing) c
selector c (ByAttrEquals t v) = not $ null $ attributeIs (Name t Nothing Nothing) v c
selector c (ByAttrContains n v) =
    case attribute (Name n Nothing Nothing) c of
        t:_ -> v `T.isInfixOf` t
        [] -> False
selector c (ByAttrStarts n v) =
    case attribute (Name n Nothing Nothing) c of
        t:_ -> v `T.isPrefixOf` t
        [] -> False
selector c (ByAttrEnds n v) =
    case attribute (Name n Nothing Nothing) c of
        t:_ -> v `T.isSuffixOf` t
        [] -> False
