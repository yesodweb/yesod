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
    -- | The type-safe URLs associated with a site argument.
    data Route a
    renderRoute :: Route a -> ([Text], [(Text, Text)])

class RenderRoute a => ParseRoute a where
    parseRoute :: ([Text], [(Text, Text)]) -> Maybe (Route a)

class RenderRoute a => RouteAttrs a where
    routeAttrs :: Route a -> Set Text
