---------------------------------------------------------
--
-- Module        : Web.Restful.Generic.ListDetail
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
-- Generic responses for listing items and then detailing them.
--
---------------------------------------------------------
module Web.Restful.Generic.ListDetail
    ( ListDetail (..)
    , ItemList (..)
    , ItemDetail (..)
    ) where

import Web.Restful.Response
import Web.Encodings
import Data.Object
import Data.Object.Instances
import Data.ByteString.Class

class ToObject a => ListDetail a where
    htmlDetail :: a -> String
    htmlDetail = fromStrictByteString . unHtml . safeFromObject . toObject
    detailTitle :: a -> String
    detailUrl :: a -> String
    htmlList :: [a] -> String
    htmlList l = "<ul>" ++ concatMap helper l ++ "</ul>"
        where
            helper i = "<li><a href=\"" ++ encodeHtml (detailUrl i) ++
                       "\">" ++ encodeHtml (detailTitle i) ++
                       "</a></li>"
    -- | Often times for the JSON response of the list, we don't need all
    -- the information.
    treeList :: [a] -> Object -- FIXME
    treeList = Sequence . map treeListSingle
    treeListSingle :: a -> Object
    treeListSingle = toObject

newtype ItemList a = ItemList [a]
instance ListDetail a => Response (ItemList a) where
    reps (ItemList l) =
        [ ("text/html", response 200 [] $ htmlList l)
        , ("application/json", response 200 [] $ unJson $ safeFromObject $ treeList l)
        ]
newtype ItemDetail a = ItemDetail a
instance ListDetail a => Response (ItemDetail a) where
    reps (ItemDetail i) =
        [ ("text/html", response 200 [] $ htmlDetail i)
        , ("application/json", response 200 [] $ unJson $ safeFromObject $ toObject i)
        ]
