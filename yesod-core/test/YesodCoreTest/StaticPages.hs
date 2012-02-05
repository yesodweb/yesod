{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module YesodCoreTest.StaticPages (specs) where

import Test.Hspec
import Test.Hspec.HUnit ()
import Test.HUnit

import Yesod.Core
import Yesod.Routes.Parse (staticPageRoutes)

data StaticPages = StaticPages

mkYesodStaticPages "StaticPages" [staticPageRoutes|
/pages/   PageR
/pages/
       about
       data
       faq
|]

instance Yesod StaticPages where approot _ = ""

handlePageR :: Handler RepHtml
handlePageR = defaultLayout [whamlet|Hello World!|]

specs :: [Spec]
specs = describe "staticPageRoutePaths" [
    it "lists static page routes" $
      ["pages","pages/about","pages/data","pages/faq"] @=? staticPageRoutePaths
  ]
