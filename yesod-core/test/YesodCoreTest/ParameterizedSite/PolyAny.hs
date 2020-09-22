{-# LANGUAGE
    TypeFamilies, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses
  , OverloadedStrings, StandaloneDeriving, FlexibleInstances
  #-}
module YesodCoreTest.ParameterizedSite.PolyAny
    ( PolyAny (..)
    ) where

import Yesod.Core

-- | Parameterized without constraints
data PolyAny a = PolyAny a

mkYesod "PolyAny a" [parseRoutes|
/ HomeR GET
|]

instance Yesod (PolyAny a)

getHomeR :: Handler a Html
getHomeR = defaultLayout
    [whamlet|
        <p>
            Stub
    |]

