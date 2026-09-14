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
import qualified Route.AuthCleared as Cleared
import Yesod.Core
import Yesod.Core.Types (YesodRunnerEnv)
import Yesod.Routes.TH.Dispatch
    ( MkDispatchSettings(..), mkMDS, mkDispatchClause, mkDispatchInstance, mkYesodSubDispatchWith )
import Yesod.Routes.TH.Types
import YesodCoreTest.RuntimeHarness (assertRequest)

type RawSubsite = WaiSubsite
type SubsiteAlias a = a

$(pure [])

mountOptionFailures :: [[[Bool]]]
mountOptionFailures = $(do
    let mount sub = ResourceLeaf (Resource "MountR" [] (Subsite sub "getSub") [] True)
        resources sub = [ResourceParent "MountParentR" True mempty [] [mount sub]]
        generate opts sub =
            [ mkDispatchInstance opts (ConT ''()) [] NoTyArgs pure [mount sub]
            , mkDispatchInstance opts (ConT ''()) [] NoTyArgs pure (resources sub)
            , mkDispatchInstance (setFocusOnNestedRoute "MountParentR" opts)
                (ConT ''()) [] NoTyArgs pure (resources sub)
            , let arg = mkName "a" in mkDispatchInstance opts (ConT ''()) []
                (toTyArgs [(VarT arg, arg)]) pure (resources sub)
            ]
        options =
            [ defaultOpts
            , setRouteHandlerWrapper (\handler _ -> handler) defaultOpts
            , setRouteAuthorization RouteAuthPerResource defaultOpts
            , setRouteAuthorization RouteAuthSubtree defaultOpts
            , setRouteHandlerWrapper (\handler _ -> handler) $
                setRouteAuthorization RouteAuthPerResource defaultOpts
            ]
        types =
            [ ConT ''WaiSubsite
            , ConT ''RawSubsite
            , ConT ''SubsiteAlias `AppT` ConT ''WaiSubsite
            , ConT ''WaiSubsiteWithAuth
            ]
        rejected action = recover [| True |] (action >> [| False |])
    listE [listE [listE (map rejected (generate opts sub)) | opts <- options] | sub <- types])

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
    let opts = setRouteHandlerWrapper (\_ _ -> fail "dispatch callback ran in a data splice") $
            setRouteAuthorization RouteAuthPerResource defaultOpts
        resources = [parseRoutes|
/ DataR GET
/mount MountR WaiSubsite getSub
|]
    _ <- mkYesodDataOpts opts "OnlyData" resources
    _ <- mkYesodSubDataOpts opts "OnlySubData" resources
    [| True |])

namedRunnerCount :: Int
namedRunnerCount = $(do
    ref <- runIO $ newIORef (0 :: Int)
    let settings = (mkMDS pure
            (runIO (modifyIORef' ref (+ 1)) >> [| yesodRunner |])
            [| error "no subsites" |]) { mdsRouteAuth = RouteAuthPerResource }
    _ <- mkDispatchClause NoTyArgs settings
        [ResourceLeaf (Resource "ManyR" [] (Methods Nothing ["GET", "POST", "PUT", "DELETE"]) [] True)]
    count <- runIO $ readIORef ref
    litE $ IntegerL $ fromIntegral count)

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
    it "rejects unguarded mounts through flat, inline, and nested dispatch, including aliases" $
        mountOptionFailures `shouldBe`
            [ replicate 4 False : replicate 4 (replicate 4 True)
            , replicate 4 False : replicate 4 (replicate 4 True)
            , replicate 4 False : replicate 4 (replicate 4 True)
            , [replicate 4 False, replicate 4 True, replicate 4 False, replicate 4 False, replicate 4 False]
            ]
    it "invokes the wrapper once per resource rather than per method or 405" $
        callbackCount `shouldBe` 2
    it "does not run dispatch callbacks or mount validation in data-only splices" $
        dataOnlySkipsWrapper `shouldBe` True
    it "generates one runner per named leaf and one for 404, regardless of method count" $
        namedRunnerCount `shouldBe` 2
    it "can clear a shared wrapper and serve an authorized read" $
        assertRequest Cleared.app "GET" 200 [] (Just "cleared")
    it "retains named authorization after clearing a wrapper, including a 405" $
        assertRequest Cleared.app "POST" 403 [] Nothing
    it "retains a custom runner when authorization succeeds" $ do
        ref <- newIORef []
        assertRequest (toWaiApp (CustomApp ref)) "GET" 200 [] (Just "custom")
        readIORef ref `shouldReturn` ["runner", "auth", "handler"]
    it "retains a custom runner when authorization denies a method mismatch" $ do
        ref <- newIORef []
        assertRequest (toWaiApp (CustomApp ref)) "POST" 403 [] Nothing
        readIORef ref `shouldReturn` ["runner", "auth"]
