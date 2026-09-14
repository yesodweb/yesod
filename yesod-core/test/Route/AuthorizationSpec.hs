{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Route.AuthorizationSpec (spec) where

import Data.IORef
import Language.Haskell.TH
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)
import Yesod.Core
import Yesod.Core.Types (YesodRunnerEnv)
import Yesod.Routes.TH.Dispatch
    ( MkDispatchSettings(..), mkMDS, mkDispatchClause, mkDispatchInstance, mkYesodSubDispatchWith )
import Yesod.Routes.TH.RenderRoute (roRouteAuth)
import Yesod.Routes.TH.Types
import YesodCoreTest.RuntimeHarness (assertRequest)

-- Exercise all public subsite dispatch entry points. The default-options
-- control must succeed, so an unrelated generator failure cannot satisfy the
-- rejection assertions.
subsiteOptionFailures :: [[Bool]]
subsiteOptionFailures = $(do
    let resources = [ResourceParent "AuthParentR" True mempty []
            [ResourceLeaf (Resource "AuthLeafR" [] (Methods Nothing ["GET"]) [] True)]]
        generate opts =
            [ mkYesodSubDispatchWith opts resources >> pure ()
            , mkYesodSubDispatchInstanceOpts opts "AuthSub" resources >> pure ()
            , mkNestedSubDispatchInstance opts "AuthParentR" [] NoTyArgs pure resources >> pure ()
            ]
        options =
            [ defaultOpts
            , setRouteAuthorization RouteAuthPerResource defaultOpts
            , setRouteAuthorization RouteAuthSubtree defaultOpts
            , setRouteHandlerWrapper (\handler _ -> handler) defaultOpts
            ]
        rejected action = recover [| True |] (action >> [| False |])
    listE [listE (map rejected (generate opts)) | opts <- options])

callbackCount :: Int
callbackCount = $(do
    ref <- runIO $ newIORef (0 :: Int)
    let opts = setRouteHandlerWrapper
            (\handler _ -> runIO (modifyIORef' ref (+ 1)) >> handler) defaultOpts
        resources =
            [ ResourceLeaf (Resource "MultiMethodR" [] (Methods Nothing ["GET", "POST"]) [] True)
            , ResourceLeaf (Resource "AnyMethodR" [] (Methods Nothing []) [] True)
            ]
    _ <- mkDispatchInstance opts (ConT ''()) [] NoTyArgs pure resources
    count <- runIO $ readIORef ref
    litE $ IntegerL $ fromIntegral count)

dataOnlySkipsWrapper :: Bool
dataOnlySkipsWrapper = $(recover [| False |] $ do
    let opts = setRouteHandlerWrapper (\_ _ -> fail "dispatch callback ran in a data splice") defaultOpts
    _ <- mkYesodDataOpts opts "OnlyData" [parseRoutes| / DataR GET |]
    _ <- mkYesodSubDataOpts opts "OnlySubData" [parseRoutes| / DataR GET |]
    [| True |])

clearedWrapperKeepsPolicy :: Bool
clearedWrapperKeepsPolicy = $(recover [| False |] $ do
    let opts = unsetRouteHandlerWrapper $
            setRouteHandlerWrapper (\_ _ -> fail "cleared wrapper ran") $
            setRouteAuthorization RouteAuthPerResource defaultOpts
    _ <- mkDispatchInstance opts (ConT ''()) [] NoTyArgs pure
        [ResourceLeaf (Resource "ClearedR" [] (Methods Nothing ["GET"]) [] True)]
    if roRouteAuth opts == RouteAuthPerResource then [| True |] else [| False |])

data CustomApp = CustomApp (IORef [String])

mkYesodData "CustomApp" [parseRoutes| / CustomR GET |]

instance Yesod CustomApp where
    messageLoggerSource = mempty
    makeSessionBackend _ = pure Nothing

record :: String -> HandlerFor CustomApp ()
record event = do
    CustomApp ref <- getYesod
    liftIO $ modifyIORef' ref (++ [event])

customRunner
    :: ToTypedContent a
    => HandlerFor CustomApp a -> YesodRunnerEnv CustomApp -> Maybe (Route CustomApp) -> Application
customRunner handler = yesodRunner (record "runner" >> handler)

do
    let settings = (mkMDS pure [| customRunner |] [| error "no subsites in this fixture" |])
            { mdsRouteAuth = RouteAuthPerResource }
    (_, clause) <- mkDispatchClause NoTyArgs settings
        [ResourceLeaf (Resource "CustomR" [] (Methods Nothing ["GET"]) [] True)]
    pure [InstanceD Nothing [] (ConT ''YesodDispatch `AppT` ConT ''CustomApp)
        [FunD 'yesodDispatch [clause]]]

getCustomR :: HandlerFor CustomApp String
getCustomR = record "handler" >> pure "custom"

authorizeCustomR :: RouteAuthorizer CustomApp
authorizeCustomR = RouteAuthorizer $ \isWrite -> do
    record "auth"
    pure $ if isWrite then Unauthorized "no writes" else Authorized

spec :: Spec
spec = describe "authorization code generation" $ do
    it "keeps default subsite dispatch and rejects unsupported authorization options" $
        subsiteOptionFailures `shouldBe`
            [replicate 3 False, replicate 3 True, replicate 3 True, replicate 3 True]
    it "invokes the wrapper once per resource rather than per method or 405" $
        callbackCount `shouldBe` 2
    it "does not run dispatch callbacks in data-only splices" $
        dataOnlySkipsWrapper `shouldBe` True
    it "can clear a shared wrapper while retaining named authorization" $
        clearedWrapperKeepsPolicy `shouldBe` True
    it "retains a custom runner when authorization succeeds" $ do
        ref <- newIORef []
        assertRequest (toWaiApp (CustomApp ref)) "GET" 200 [] (Just "custom")
        readIORef ref `shouldReturn` ["runner", "auth", "handler"]
    it "retains a custom runner when authorization denies a method mismatch" $ do
        ref <- newIORef []
        assertRequest (toWaiApp (CustomApp ref)) "POST" 403 [] Nothing
        readIORef ref `shouldReturn` ["runner", "auth"]
