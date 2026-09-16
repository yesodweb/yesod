{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module YesodCoreTest.RouteAuthLogin (specs) where

import Control.Monad (forM_)
import Data.IORef
import qualified Data.Map as Map
import Data.Text (Text)
import Network.HTTP.Types (http11)
import qualified Network.Wai as W
import qualified Network.Wai.Test as WT
import Test.Hspec
import Yesod.Core

data LoginSub = LoginSub

mkYesodSubData "LoginSub" [parseRoutes| /page PageR GET |]

data LoginApp = LoginApp Bool (IORef SessionMap)

mkYesodOpts (setRouteAuthorization RouteAuthPerResource defaultOpts) "LoginApp" [parseRoutes|
/login LoginR GET
/private/#Int PrivateR GET
/mount/#Int MountR LoginSub getLoginSub
/nested NestedR:
    /private/#Int NestedPrivateR GET
    /mount/#Int NestedMountR LoginSub getLoginSub
|]

instance YesodSubDispatch LoginSub LoginApp where
    yesodSubDispatch = $(mkYesodSubDispatch [parseRoutes| /page PageR GET |])

getLoginSub :: LoginApp -> Int -> LoginSub
getLoginSub _ _ = LoginSub

getPageR :: SubHandlerFor LoginSub LoginApp Text
getPageR = pure "private subsite handler must not run"

authorizeMountR, authorizeNestedMountR :: Int -> Bool -> HandlerFor LoginApp AuthResult
authorizeMountR = authorizePrivateR
authorizeNestedMountR = authorizePrivateR

instance Yesod LoginApp where
    messageLoggerSource = mempty
    authRoute (LoginApp enabled _) = if enabled then Just LoginR else Nothing
    makeSessionBackend (LoginApp _ ref) = pure $ Just $ SessionBackend $ \_ -> do
        session <- readIORef ref
        pure (session, \saved -> writeIORef ref saved >> pure [])

authorizeLoginR :: Bool -> HandlerFor LoginApp AuthResult
authorizeLoginR _ = pure Authorized

authorizePrivateR, authorizeNestedPrivateR :: Int -> Bool -> HandlerFor LoginApp AuthResult
authorizePrivateR _ _ = pure AuthenticationRequired
authorizeNestedPrivateR = authorizePrivateR

getLoginR :: HandlerFor LoginApp Text
getLoginR = pure "login"

getPrivateR, getNestedPrivateR :: Int -> HandlerFor LoginApp Text
getPrivateR _ = pure "private handler must not run"
getNestedPrivateR = getPrivateR

specs :: Spec
specs = describe "named authentication requirements" $
    forM_ [(path, matched) | prefix <- ["", "/nested"],
            (suffix, matched) <- [("/private/7", True), ("/mount/7/page", True), ("/mount/7/missing", False)],
            let path = prefix <> suffix] $ \(path, matched) ->
        forM_ [True, False] $ \hasLogin ->
            forM_ ["text/html", "application/json"] $ \accept ->
                it (show (path, hasLogin, accept)) $ do
                    session <- newIORef (Map.singleton "_ULT" "/previous")
                    app <- toWaiApp (LoginApp hasLogin session)
                    let redirects = hasLogin && accept == "text/html"
                        url = path <> "?next=here"
                        req = (WT.setPath WT.defaultRequest url)
                            { W.requestHeaders = [("Accept", accept)]
                            , W.httpVersion = http11
                            }
                    WT.runSession (do
                        response <- WT.request req
                        WT.assertStatus (if redirects then 303 else 401) response
                        liftIO $ lookup "Location" (WT.simpleHeaders response)
                            `shouldBe` (if redirects then Just "/login" else Nothing)) app
                    Map.lookup "_ULT" <$> readIORef session `shouldReturn`
                        Just (if redirects && matched then url else "/previous")
