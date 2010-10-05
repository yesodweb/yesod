
--------------------------------------------------------------------------------
-- |
-- Module      : Text.XRDS
-- Copyright   : (c) Trevor Elliott, 2008
-- License     : BSD3
--
-- Maintainer  : Trevor Elliott <trevor@geekgateway.com>
-- Stability   :
-- Portability :
--

module OpenId2.XRDS (
    -- * Types
    XRDS, XRD
  , Service(..)

    -- * Utility Functions
  , isUsable
  , hasType

    -- * Parsing
  , parseXRDS
  ) where

-- Libraries
import Control.Arrow
import Control.Monad
import Data.List
import Data.Maybe
import Text.XML.Light


-- Types -----------------------------------------------------------------------

type XRDS = [XRD]

type XRD = [Service]

data Service = Service
  { serviceTypes      :: [String]
  , serviceMediaTypes :: [String]
  , serviceURIs       :: [String]
  , serviceLocalIDs   :: [String]
  , servicePriority   :: Maybe Int
  , serviceExtra      :: [Element]
  } deriving Show

-- Utilities -------------------------------------------------------------------

-- | Check to see if an XRDS service description is usable.
isUsable :: XRDS -> Bool
isUsable  = not . null . concat


-- | Generate a tag name predicate, that ignores prefix and namespace.
tag :: String -> Element -> Bool
tag n el = qName (elName el) == n


-- | Filter the attributes of an element by some predicate
findAttr' :: (QName -> Bool) -> Element -> Maybe String
findAttr' p el = attrVal `fmap` find (p . attrKey) (elAttribs el)


-- | Read, maybe
readMaybe :: Read a => String -> Maybe a
readMaybe str = case reads str of
  [(x,"")] -> Just x
  _        -> Nothing


-- | Get the text of an element
getText :: Element -> String
getText el = case elContent el of
  [Text cd] -> cdData cd
  _         -> []


-- | Generate a predicate over Service Types.
hasType :: String -> Service -> Bool
hasType ty svc = ty `elem` serviceTypes svc


-- Parsing ---------------------------------------------------------------------


parseXRDS :: String -> Maybe XRDS
parseXRDS str = do
  doc <- parseXMLDoc str
  let xrds = filterChildren (tag "XRD") doc
  return $ map parseXRD xrds


parseXRD :: Element -> XRD
parseXRD el =
  let svcs = filterChildren (tag "Service") el
   in mapMaybe parseService svcs


parseService :: Element -> Maybe Service
parseService el = do
  let vals t x = first (map getText) $ partition (tag t) x
      (tys,tr)    = vals "Type"      (elChildren el)
      (mts,mr)    = vals "MediaType" tr
      (uris,ur)   = vals "URI"       mr
      (lids,rest) = vals "LocalID"   ur
      priority = readMaybe =<< findAttr' (("priority" ==) . qName) el
  guard $ not $ null tys
  return $ Service { serviceTypes      = tys
                   , serviceMediaTypes = mts
                   , serviceURIs       = uris
                   , serviceLocalIDs   = lids
                   , servicePriority   = priority
                   , serviceExtra      = rest
                   }
