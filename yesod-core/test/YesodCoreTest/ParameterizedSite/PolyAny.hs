{-# LANGUAGE
    TypeFamilies, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses
  , OverloadedStrings, StandaloneDeriving, FlexibleInstances
  #-}
module YesodCoreTest.ParameterizedSite.PolyAny
    ( PolyAny (..)
    ) where

import Yesod.Core
import Yesod.Routes.TH.Types (ResourceTree)

-- | Parameterized without constraints
data PolyAny a = PolyAny a

mkYesod "PolyAny a" [parseRoutes|
/ HomeR GET
|]

_unused :: [ResourceTree String]
_unused = resourcesPolyAny

instance Yesod (PolyAny a)

getHomeR :: Handler a Html
getHomeR = defaultLayout
    [whamlet|
        <p>
            Stub
    |]
