{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-} -- the module name is a lie!!!
module YesodCoreTest.NoOverloadedStrings (noOverloadedTest, Widget) where

import Test.Hspec
import YesodCoreTest.NoOverloadedStringsSub

import Yesod.Core
import Network.Wai
import Network.Wai.Test
import Data.Monoid (mempty)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as L8

getSubsite :: a -> Subsite
getSubsite _ = Subsite $(mkYesodSubDispatch resourcesSubsite)

getBarR :: Monad m => m T.Text
getBarR = return $ T.pack "BarR"

getBazR :: Yesod master => HandlerT Subsite (HandlerT master IO) Html
getBazR = lift $ defaultLayout [whamlet|Used Default Layout|]

getBinR :: Yesod master => HandlerT Subsite (HandlerT master IO) Html
getBinR = do
    widget <- widgetToParentWidget [whamlet|
        <p>Used defaultLayoutT
        <a href=@{BazR}>Baz
    |]
    lift $ defaultLayout widget

getOnePiecesR :: Monad m => Int -> m ()
getOnePiecesR _ = return ()

getTwoPiecesR :: Monad m => Int -> Int -> m ()
getTwoPiecesR _ _ = return ()

getThreePiecesR :: Monad m => Int -> Int -> Int -> m ()
getThreePiecesR _ _ _ = return ()

data Y = Y
mkYesod "Y" [parseRoutes|
/ RootR GET
/foo FooR GET
/subsite SubsiteR Subsite getSubsite
|]

instance Yesod Y

getRootR :: Handler ()
getRootR = return ()

getFooR :: Handler ()
getFooR = return ()

runner :: Session () -> IO ()
runner f = toWaiApp Y >>= runSession f

case_sanity :: IO ()
case_sanity = runner $ do
    res <- request defaultRequest
    assertBody mempty res

case_subsite :: IO ()
case_subsite = runner $ do
    res <- request defaultRequest
        { pathInfo = map T.pack ["subsite", "bar"]
        }
    assertBody (L8.pack "BarR") res
    assertStatus 200 res

case_deflayout :: IO ()
case_deflayout = runner $ do
    res <- request defaultRequest
        { pathInfo = map T.pack ["subsite", "baz"]
        }
    assertBodyContains (L8.pack "Used Default Layout") res
    assertStatus 200 res

case_deflayoutT :: IO ()
case_deflayoutT = runner $ do
    res <- request defaultRequest
        { pathInfo = map T.pack ["subsite", "bin"]
        }
    assertBodyContains (L8.pack "Used defaultLayoutT") res
    assertStatus 200 res

noOverloadedTest :: Spec
noOverloadedTest = describe "Test.NoOverloadedStrings" $ do
      it "sanity" case_sanity
      it "subsite" case_subsite
      it "deflayout" case_deflayout
      it "deflayoutT" case_deflayoutT
