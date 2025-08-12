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

-- | This class acts as a delegation class for 'RenderRoute' on nested
-- route fragments.
--
-- @since TODO
class Eq a => RenderRouteNested a where
    -- | Render the fragment of the route. To form a complete route, you'll
    -- need to `mappend` this with the result from the parent `renderRoute`
    -- or `renderRouteNested` call.
    --
    -- @since TODO
    renderRouteNested :: a -> ([Text], [(Text, Text)])

class RenderRoute a => ParseRoute a where
    parseRoute :: ([Text], [(Text, Text)]) -- ^ The path of the URL split on forward slashes, and a list of query parameters with their associated value.
               -> Maybe (Route a)

-- | Like 'RenderRouteNested', this acts as a delegation class for nested
-- route fragments.
--
-- @since TODO
class RenderRouteNested a => ParseRouteNested a where
    -- |
    --
    -- @since TODO
    parseRouteNested
        :: ([Text], [(Text, Text)])
        -- ^ The path of the URL split on forward slashes, and a list of query parameters with their associated value.
        --
        -- Unlike for 'parseRoute', this will not include URL fragments as
        -- part of parent routes. It is expected that parents will take the
        -- resulting @a@ and construct the parent from that.
        -> Maybe a

class RenderRoute a => RouteAttrs a where
    routeAttrs :: Route a
               -> Set Text -- ^ A set of <http://www.yesodweb.com/book/route-attributes attributes associated with the route>.

-- | Like 'RenderRouteNested', this acts as a delegation class for nested
-- route fragments to provide 'RouteAttrs'.
--
-- @since TODO
class RenderRouteNested a => RouteAttrsNested a where
    -- | Retrieve the 'RouteAttrs' for a given route fragment.
    --
    -- @since TODO
    routeAttrsNested :: a -> Set Text
