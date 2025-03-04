{-# LANGUAGE
    TypeFamilies, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses
  , OverloadedStrings, StandaloneDeriving, FlexibleInstances
  #-}
module YesodCoreTest.ParameterizedSite.Compat
    ( Compat (..)
    ) where

import Yesod.Core
import Yesod.Routes.TH.Types (ResourceTree)

-- | Parameterized without constraints, and we call mkYesod without type vars,
-- like people used to do before the last 3 commits
data Compat a b = Compat a b

mkYesod "Compat" [parseRoutes|
/ HomeR GET
|]

_unused :: [ResourceTree String]
_unused = resourcesCompat

instance Yesod (Compat a b)

getHomeR :: Handler a b Html
getHomeR = defaultLayout
    [whamlet|
        <p>
            Stub
    |]
