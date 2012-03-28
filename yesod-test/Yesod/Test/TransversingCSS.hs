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
  queryToArrow,
  Selector(..),
  SelectorGroup(..)

  )
where

import Text.XML.HXT.Core
import qualified Data.List as DL
import Yesod.Test.CssQuery
import Text.ParserCombinators.Parsec (ParseError)

type Html = String
type Query = String
 
-- | Perform a css 'Query' on 'Html'. Returns Either
--
-- * Left: Query parse error.
--
-- * Right: List of matching Html fragments.
findBySelector :: Html-> Query -> Either ParseError [Html]
findBySelector html query = fmap (runQuery html) (parseQuery query)

-- Run a compiled query on Html, returning a list of matching Html fragments.
runQuery :: Html -> [[SelectorGroup]] -> [Html]
runQuery html query =
  runLA (hread >>> (queryToArrow query) >>> xshow this) html

-- | Transform a compiled query into the HXT arrow that finally transverses the Html
queryToArrow :: ArrowXml a => [[SelectorGroup]] -> a XmlTree XmlTree
queryToArrow commaSeparated = 
  DL.foldl uniteCommaSeparated none commaSeparated
 where
  uniteCommaSeparated accum selectorGroups =
    accum <+> (DL.foldl sequenceSelectorGroups this selectorGroups)
  sequenceSelectorGroups accum (DirectChildren sels) =
    accum >>> getChildren >>> (DL.foldl applySelectors this $ sels)
  sequenceSelectorGroups accum (DeepChildren sels) =
    accum >>> getChildren >>> multi (DL.foldl applySelectors this $ sels)
  applySelectors accum selector = accum >>> (toArrow selector)
  toArrow selector = case selector of
    ById v -> hasAttrValue "id" (==v)
    ByClass v -> hasAttrValue "class" ((DL.elem v) . words)
    ByTagName v -> hasName v
    ByAttrExists n -> hasAttr n
    ByAttrEquals n v -> hasAttrValue n (==v)
    ByAttrContains n v -> hasAttrValue n (DL.isInfixOf v)
    ByAttrStarts n v -> hasAttrValue n (DL.isPrefixOf v)
    ByAttrEnds n v -> hasAttrValue n (DL.isSuffixOf v)
