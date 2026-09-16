{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module InlineCompat where
import Yesod.Core

data Compat a = Compat
mkYesodDataOpts (setRouteLeafViews True defaultOpts) "Compat a" [parseRoutes|
/child ChildR:
    / ChildHomeR GET
|]
