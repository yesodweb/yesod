{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-} -- the module name is a lie!!!
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}

module YesodCoreTest.NoOverloadedStrings
    ( noOverloadedTest
    , Widget
    , resourcesY
    ) where

import Test.Hspec
import YesodCoreTest.NoOverloadedStringsSub

import Yesod.Core
import Network.Wai
import Network.Wai.Test
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as L8

getSubsite :: a -> Subsite
getSubsite _ = Subsite $(mkYesodSubDispatch resourcesSubsite)

getBarR :: MonadHandler m => m T.Text
getBarR = return $ T.pack "BarR"

getBazR :: (MonadHandler m, Yesod (HandlerSite m)) => m Html
getBazR = liftHandler $ defaultLayout [whamlet|Used Default Layout|]

getBinR :: (MonadHandler m, Yesod (HandlerSite m), SubHandlerSite m ~ Subsite) => m Html
getBinR = do
    routeToParent <- getRouteToParent
    liftHandler $ defaultLayout [whamlet|
        <p>Used defaultLayoutT
        <a href=@{routeToParent BazR}>Baz
    |]

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
