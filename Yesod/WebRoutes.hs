-- | This module should be removed when web-routes incorporates necessary support.
module Yesod.WebRoutes
    ( encodePathInfo
    , Site (..)
    ) where

import Web.Routes.Base (encodePathInfo)
import Web.Routes.Site (Site (..))
