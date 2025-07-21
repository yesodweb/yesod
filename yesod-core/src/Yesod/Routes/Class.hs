{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Yesod.Routes.Class
    ( RenderRoute (..)
    , ParseRoute (..)
    , RouteAttrs (..)
    , -- * For nested routes and splitting files
      RenderRouteNested (..)
    , ParseRouteNested (..)
    , RouteAttrsNested (..)
    ) where

import Data.Text (Text)
import Data.Set (Set)

class Eq (Route a) => RenderRoute a where
    -- | The <http://www.yesodweb.com/book/routing-and-handlers type-safe URLs> associated with a site argument.
    data Route a
    renderRoute :: Route a
                -> ([Text], [(Text, Text)]) -- ^ The path of the URL split on forward slashes, and a list of query parameters with their associated value.

class Eq a => RenderRouteNested a where
    renderRouteNested :: a -> ([Text], [(Text, Text)])

class RenderRoute a => ParseRoute a where
    parseRoute :: ([Text], [(Text, Text)]) -- ^ The path of the URL split on forward slashes, and a list of query parameters with their associated value.
               -> Maybe (Route a)

class RenderRouteNested a => ParseRouteNested a where
    parseRouteNested :: ([Text], [(Text, Text)]) -> Maybe a

class RenderRoute a => RouteAttrs a where
    routeAttrs :: Route a
               -> Set Text -- ^ A set of <http://www.yesodweb.com/book/route-attributes attributes associated with the route>.

class RenderRouteNested a => RouteAttrsNested a where
    routeAttrsNested :: a -> Set Text
