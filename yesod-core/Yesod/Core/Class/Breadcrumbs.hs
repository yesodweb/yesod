{-# LANGUAGE OverloadedStrings #-}
module Yesod.Core.Class.Breadcrumbs where

import Yesod.Core.Handler
import Yesod.Routes.Class
import Data.Text (Text)

-- | A type-safe, concise method of creating breadcrumbs for pages. For each
-- resource, you declare the title of the page and the parent resource (if
-- present).
class YesodBreadcrumbs site where
    -- | Returns the title and the parent resource, if available. If you return
    -- a 'Nothing', then this is considered a top-level page.
    breadcrumb :: Route site -> HandlerT site IO (Text , Maybe (Route site))

-- | Gets the title of the current page and the hierarchy of parent pages,
-- along with their respective titles.
breadcrumbs :: YesodBreadcrumbs site => HandlerT site IO (Text, [(Route site, Text)])
breadcrumbs = do
    x <- getCurrentRoute
    case x of
        Nothing -> return ("Not found", [])
        Just y -> do
            (title, next) <- breadcrumb y
            z <- go [] next
            return (title, z)
  where
    go back Nothing = return back
    go back (Just this) = do
        (title, next) <- breadcrumb this
        go ((this, title) : back) next
