{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module YesodCoreTest.ParameterizedSite.Compat
    ( Compat (..)
    ) where

import Yesod.Core

-- | Parameterized without constraints, and we call mkYesod without type vars,
-- like people used to do before the last 3 commits
data Compat a b = Compat a b

mkYesod "Compat" [parseRoutes|
/ HomeR GET
|]

instance Yesod (Compat a b)

getHomeR :: Handler a b Html
getHomeR = defaultLayout
    [whamlet|
        <p>
            Stub
    |]
