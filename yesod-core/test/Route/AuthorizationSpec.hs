{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Route.AuthorizationSpec (spec) where

import Data.IORef
import Control.Monad (forM_)
import Language.Haskell.TH
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)
import qualified Route.AuthCleared as Cleared
import Yesod.Core
import Yesod.Core.Types (YesodRunnerEnv)
import Yesod.Routes.TH.Dispatch
    ( MkDispatchSettings(..), mkMDS, mkDispatchClause, mkDispatchInstance, mkYesodSubDispatchWith )
import Yesod.Routes.TH.Types
import Yesod.Routes.TH.RenderRoute (roSiteAuthorization)
import YesodCoreTest.RuntimeHarness (assertRequest)

type RawSubsite = WaiSubsite
type SubsiteAlias a = a

type family RawFamily a where
    RawFamily () = WaiSubsite
type family NullaryRawFamily where
    NullaryRawFamily = WaiSubsite
type family SafeFamily a where
    SafeFamily () = WaiSubsiteWithAuth
type FamilyAlias = RawFamily ()

$(pure [])

mountOptionFailures :: [(String, [(String, Bool, Bool)])]
mountOptionFailures = $(do
    let mount sub = ResourceLeaf (Resource "MountR" [] (Subsite sub "getSub") [] True)
        resources sub = [ResourceParent "MountParentR" True mempty [] [mount sub]]
        generate :: RouteOpts -> Type -> [(String, DecsQ)]
        generate opts sub =
            [ ("flat", mkDispatchInstance opts (ConT ''()) [] NoTyArgs pure [mount sub])
            , ("nested", mkDispatchInstance opts (ConT ''()) [] NoTyArgs pure (resources sub))
            , ("focused", mkDispatchInstance (setFocusOnNestedRoute "MountParentR" opts)
                (ConT ''()) [] NoTyArgs pure (resources sub))
            , ("inline", let arg = mkName "a" in mkDispatchInstance opts (ConT ''()) []
                (toTyArgs [(VarT arg, arg)]) pure (resources sub))
            ]
        options :: Bool -> [(String, RouteOpts, Bool)]
        options rejectsNamed =
            [ ("defaults", defaultOpts, False)
            , ("wrapper only", setRouteHandlerWrapper (\handler _ -> handler) defaultOpts, True)
            , ("per resource", setRouteAuthorization RouteAuthPerResource defaultOpts, rejectsNamed)
            , ("subtree", setRouteAuthorization RouteAuthSubtree defaultOpts, rejectsNamed)
            , ("named and wrapper", setRouteHandlerWrapper (\handler _ -> handler) $
                setRouteAuthorization RouteAuthPerResource defaultOpts, rejectsNamed)
            ]
        -- The two concrete types exercise every generator path. Alias/name/
        -- family cases test the shared validator once per option, with defaults
        -- as controls. Keep the expected result beside its type and label.
        types :: [(String, Type, Bool, Bool)]
        types =
            [ ("raw WAI", ConT ''WaiSubsite, True, True)
            , ("ordinary raw alias", ConT ''RawSubsite, True, False)
            , ("applied raw alias", ConT ''SubsiteAlias `AppT` ConT ''WaiSubsite, True, False)
            , ("repeated raw alias", ConT ''SubsiteAlias `AppT` (ConT ''SubsiteAlias `AppT` ConT ''WaiSubsite), True, False)
            , ("WAI with auth", ConT ''WaiSubsiteWithAuth, False, True)
            , ("repeated safe alias", ConT ''SubsiteAlias `AppT` (ConT ''SubsiteAlias `AppT` ConT ''WaiSubsiteWithAuth), False, False)
            , ("unresolved bare name", ConT (mkName "NotImportedSub"), True, False)
            , ("unresolved qualified name", ConT (mkName "Missing.WaiSubsite"), True, False)
            , ("type variable", VarT (mkName "sub"), True, False)
            , ("family alias", ConT ''FamilyAlias, True, False)
            , ("nullary family", ConT ''NullaryRawFamily, True, False)
            , ("safe family", ConT ''SafeFamily `AppT` ConT ''(), True, False)
            ]
        rejected action = recover [| True |] (action >> [| False |])
    listE [ [| (label, $(listE
        [ [| (option ++ "/" ++ path, $(rejected action), expected) |]
        | (option, opts, expected) <- options rejectsNamed
        , (path, action) <- (if allPaths then id else take 1) (generate opts sub)
        ])) |]
        | (label, sub, rejectsNamed, allPaths) <- types ])

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
            , subsiteRouteOpts $ setRouteHandlerWrapper (\_ _ -> fail "cleared callback ran") $
                setRouteAuthorization RouteAuthPerResource defaultOpts
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
            [| error "no subsites" |])
            { mdsSiteAuthorization = roSiteAuthorization $ setRouteAuthorization RouteAuthPerResource defaultOpts }
    _ <- mkDispatchClause NoTyArgs settings
        [ResourceLeaf (Resource "ManyR" [] (Methods Nothing ["GET", "POST", "PUT", "DELETE"]) [] True)]
    count <- runIO $ readIORef ref
    litE $ IntegerL $ fromIntegral count)

untypedMountFailures :: [Bool]
untypedMountFailures = $(do
    let generate policy = mkDispatchClause NoTyArgs
            ((mkMDS pure [| yesodRunner |] [| error "unused" |])
                { mdsSiteAuthorization = roSiteAuthorization $ setRouteAuthorization policy defaultOpts })
            [ResourceLeaf (Resource "MountR" [] (Subsite "WaiSubsite" "getSub") [] True)]
    listE [recover [| True |] (generate policy >> [| False |])
        | policy <- [NoRouteAuth, RouteAuthPerResource, RouteAuthSubtree]])

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
            { mdsSiteAuthorization = roSiteAuthorization $ setRouteAuthorization RouteAuthPerResource defaultOpts }
    (_, clause) <- mkDispatchClause NoTyArgs settings
        [ResourceLeaf (Resource "CustomR" [] (Methods Nothing ["GET"]) [] True)]
    pure [InstanceD Nothing [] (ConT ''YesodDispatch `AppT` ConT ''CustomApp)
        [FunD 'yesodDispatch [clause]]]

getCustomR :: HandlerFor CustomApp String
getCustomR = record "handler" >> pure "custom"

authorizeCustomR :: Bool -> HandlerFor CustomApp AuthResult
authorizeCustomR isWrite = do
    record "auth"
    pure $ if isWrite then Unauthorized "no writes" else Authorized

spec :: Spec
spec = describe "authorization code generation" $ do
    it "keeps default subsite dispatch and rejects unsupported authorization options" $
        subsiteOptionFailures `shouldBe`
            [replicate 3 False, replicate 3 True, replicate 3 True, replicate 3 True, replicate 3 False]
    forM_ mountOptionFailures $ \(label, results) ->
        describe ("mount: " ++ label) $
            forM_ results $ \(scenario, actual, expected) ->
                it scenario $ actual `shouldBe` expected
    it "rejects named mounts when the generic generator cannot validate their types" $
        untypedMountFailures `shouldBe` [False, True, True]
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
