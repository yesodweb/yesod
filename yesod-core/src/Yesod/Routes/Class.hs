{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Yesod.Routes.Class
    ( RenderRoute (..)
    , ParseRoute (..)
    , RouteAttrs (..)
    , -- * For nested routes and splitting files
      RenderRouteNested (..)
    , ParseRouteNested (..)
    , RouteAttrsNested (..)
    , WithParentArgs (..)
    ) where

import Data.Text (Text)
import Data.Set (Set)
import Data.Kind (Type)

class Eq (Route a) => RenderRoute a where
    -- | The <http://www.yesodweb.com/book/routing-and-handlers type-safe URLs> associated with a site argument.
    data Route a
    renderRoute :: Route a
                -> ([Text], [(Text, Text)]) -- ^ The path of the URL split on forward slashes, and a list of query parameters with their associated value.

data WithParentArgs a = WithParentArgs { theParentArgs :: ParentArgs a, parentArgsFor :: a }

deriving stock instance (Eq (ParentArgs a), Eq a) => Eq (WithParentArgs a)

instance (Eq (ParentArgs a), RenderRouteNested a) => RenderRouteNested (WithParentArgs a) where
    type ParentSite (WithParentArgs a) = ParentSite a
    type ParentArgs (WithParentArgs a) = ()

    renderRouteNested () (WithParentArgs parentArgs a) =
        renderRouteNested parentArgs a

-- | This class acts as a delegation class for 'RenderRoute' on nested
-- route fragments.
--
-- @since 1.6.28.0
class Eq a => RenderRouteNested a where
    -- | The site type for a given route fragment.
    --
    -- @since 1.6.28.0
    type ParentSite a :: Type

    -- | The 'ParentArgs' are the route fragments necessary to call the
    -- dispatched route that are not part of the route fragments used in
    -- parsing the route.
    --
    -- @since 1.6.28.0
    type ParentArgs a :: Type
    type ParentArgs a = ()

    -- | Render the fragment of the route. To form a complete route, you'll
    -- need to `mappend` this with the result from the parent `renderRoute`
    -- or `renderRouteNested` call.
    --
    -- @since 1.6.28.0
    renderRouteNested :: ParentArgs a -> a -> ([Text], [(Text, Text)])

instance (RenderRoute site) => RenderRouteNested (Route site) where
    type ParentSite (Route site) = site
    type ParentArgs (Route site) = ()

    renderRouteNested () a = renderRoute a

class RenderRoute a => ParseRoute a where
    parseRoute :: ([Text], [(Text, Text)]) -- ^ The path of the URL split on forward slashes, and a list of query parameters with their associated value.
               -> Maybe (Route a)

-- | Like 'RenderRouteNested', this acts as a delegation class for nested
-- route fragments.
--
-- @since 1.6.28.0
class RenderRouteNested a => ParseRouteNested a where
    -- |
    --
    -- @since 1.6.28.0
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
-- @since 1.6.28.0
class RenderRouteNested a => RouteAttrsNested a where
    -- | Retrieve the 'RouteAttrs' for a given route fragment.
    --
    -- @since 1.6.28.0
    routeAttrsNested :: a -> Set Text
