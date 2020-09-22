{-# LANGUAGE
    TypeFamilies, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses
  , OverloadedStrings, StandaloneDeriving, FlexibleInstances
  #-}
module YesodCoreTest.ParameterizedSite.PolyShow
    ( PolyShow (..)
    ) where

import Yesod.Core

-- | Parameterized with 'Show' constraint
data PolyShow a = PolyShow a

mkYesod "(Show a) => PolyShow a" [parseRoutes|
/ HomeR GET
|]

instance Show a => Yesod (PolyShow a)

getHomeR :: Show a => Handler a Html
getHomeR = do
    PolyShow x <- getYesod
    defaultLayout
        [whamlet|
            <p>
                Stub #{show x}
        |]

