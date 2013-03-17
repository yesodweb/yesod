{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Yesod.Routes.Class
    ( RenderRoute (..)
    , ParseRoute (..)
    ) where

import Data.Text (Text)

class Eq (Route a) => RenderRoute a where
    -- | The type-safe URLs associated with a site argument.
    data Route a
    renderRoute :: Route a -> ([Text], [(Text, Text)])

class RenderRoute a => ParseRoute a where
    parseRoute :: ([Text], [(Text, Text)]) -> Maybe (Route a)
