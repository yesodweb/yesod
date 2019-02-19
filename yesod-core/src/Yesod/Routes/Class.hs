{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Yesod.Routes.Class
    ( RenderRoute (..)
    , ParseRoute (..)
    , RouteAttrs (..)
    ) where

import Data.Text (Text)
import Data.Set (Set)

class Eq (Route a) => RenderRoute a where
    -- | The <http://www.yesodweb.com/book/routing-and-handlers type-safe URLs> associated with a site argument.
    data Route a
    renderRoute :: Route a
                -> ([Text], [(Text, Text)]) -- ^ The path of the URL split on forward slashes, and a list of query parameters with their associated value.

class RenderRoute a => ParseRoute a where
    parseRoute :: ([Text], [(Text, Text)]) -- ^ The path of the URL split on forward slashes, and a list of query parameters with their associated value.
               -> Maybe (Route a)

class RenderRoute a => RouteAttrs a where
    routeAttrs :: Route a
               -> Set Text -- ^ A set of <http://www.yesodweb.com/book/route-attributes attributes associated with the route>.
