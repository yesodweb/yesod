{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Yesod.Routes.Class
    ( RenderRoute (..)
    ) where

import Data.Text (Text)

class Eq (Route a) => RenderRoute a where
    -- | The type-safe URLs associated with a site argument.
    data Route a
    renderRoute :: Route a -> ([Text], [(Text, Text)])
