{-# LANGUAGE OverloadedStrings #-}
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
    XRDS
  , Service(..)

    -- * Parsing
  , parseXRDS
  ) where

-- Libraries
import Control.Monad ((>=>))
import Data.Maybe (listToMaybe)
import Text.XML (parseLBS, def)
import Text.XML.Cursor (fromDocument, element, content, ($/), (&|), Cursor, (&/), attribute)
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import qualified Data.Text.Read

-- Types -----------------------------------------------------------------------

type XRDS = [XRD]

type XRD = [Service]

data Service = Service
  { serviceTypes      :: [Text]
  , serviceMediaTypes :: [Text]
  , serviceURIs       :: [Text]
  , serviceLocalIDs   :: [Text]
  , servicePriority   :: Maybe Int
  } deriving Show

parseXRDS :: L.ByteString -> Maybe XRDS
parseXRDS str =
    either
        (const Nothing)
        (Just . parseXRDS' . fromDocument)
        (parseLBS def str)

parseXRDS' :: Cursor -> [[Service]]
parseXRDS' = element "{xri://$xrds}XRDS" &/
             element "{xri://$xrd*($v*2.0)}XRD" &|
             parseXRD

parseXRD :: Cursor -> [Service]
parseXRD c = c $/ element "{xri://$xrd*($v*2.0)}Service" >=> parseService

parseService :: Cursor -> [Service]
parseService c =
    if null types then [] else [Service
        { serviceTypes = types
        , serviceMediaTypes = mtypes
        , serviceURIs = uris
        , serviceLocalIDs = localids
        , servicePriority = listToMaybe (attribute "priority" c) >>= readMaybe
        }]
  where
    types = c $/ element "{xri://$xrd*($v*2.0)}Type" &/ content
    mtypes = c $/ element "{xri://$xrd*($v*2.0)}MediaType" &/ content
    uris = c $/ element "{xri://$xrd*($v*2.0)}URI" &/ content
    localids = c $/ element "{xri://$xrd*($v*2.0)}LocalID" &/ content
    readMaybe t =
        case Data.Text.Read.signed Data.Text.Read.decimal t of
            Right (i, "") -> Just i
            _ -> Nothing
