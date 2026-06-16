{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Regression pin for invoking @mkYesod@ on a *parameterized* foundation
-- /without/ writing its explicit type argument (here @mkYesod "PNoArgs"@ for
-- @data PNoArgs a@). The reified arity is filled with a fresh type variable,
-- and that fill var must travel in the 'TyArgs' handed to 'discoveryMode' /
-- the generators — otherwise the site is misclassified as monomorphic and
-- emits ill-scoped nested instances (a 1.6 -> 1.7 regression). With the fix it
-- stays on the backwards-compatible inline path. The build is the assertion;
-- the WAI rows confirm it still dispatches.
module YesodCoreTest.ParamNoExplicitArgs
    ( specs
    ) where

import Data.Text (Text)
import Test.Hspec
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Types as H
import Yesod.Core

import YesodCoreTest.RuntimeHarness (assertRequestFor)

-- One type argument, but the splice below names only "PNoArgs" (no @a@).
data PNoArgs a = PNoArgs

-- @CatchAllR@ overlaps @SubParentR@'s prefix (both start with @sub@), so its
-- @!@ turns overlap checking off for it. It is placed AFTER the parent on
-- purpose: under the historical (1.6) inline @parseRoute@ semantics a path that
-- matches the @sub@ prefix but misses every child must COMMIT at the parent and
-- return 'Nothing', NOT fall through to this later, lower-priority sibling. That
-- is exactly what inline @dispatch@ does (404), so @parseRoute@ has to agree.
mkYesod "PNoArgs" [parseRoutes|
/ HomeR GET
/sub SubParentR:
    / SubHomeR GET
!/sub/#Text CatchAllR GET
|]

instance Yesod (PNoArgs a) where
    messageLoggerSource = mempty

getHomeR :: HandlerFor (PNoArgs a) Text
getHomeR = pure "home"

getSubHomeR :: HandlerFor (PNoArgs a) Text
getSubHomeR = pure "subHome"

getCatchAllR :: Text -> HandlerFor (PNoArgs a) Text
getCatchAllR _ = pure "catchAll"

testRequestIO
    :: HasCallStack
    => Int
    -> [Text]
    -> H.Method
    -> Maybe L.ByteString
    -> IO ()
testRequestIO status path method mexpected =
    assertRequestFor (PNoArgs :: PNoArgs ()) method status path mexpected

specs :: Spec
specs = describe "parameterized foundation invoked without explicit type args" $ do
    it "static leaf dispatches" $
        testRequestIO 200 [] "GET" (Just "home")
    it "inlined nested leaf dispatches" $
        testRequestIO 200 ["sub"] "GET" (Just "subHome")
    it "404 on unknown nested suffix (dispatch commits at the parent prefix)" $
        testRequestIO 404 ["sub", "oops"] "GET" Nothing
    it "parseRoute commits at the parent prefix: a child miss is Nothing, not the later overlapping sibling" $
        -- The regression: with overlap checking off the flat inline codegen let
        -- this fall through to @CatchAllR@, so parseRoute returned @Just
        -- (CatchAllR "oops")@ while dispatch returned 404 — they disagreed for
        -- the same request. Commit-on-parent-prefix makes parseRoute agree.
        (parseRoute (["sub", "oops"], []) :: Maybe (Route (PNoArgs ())))
            `shouldBe` Nothing
    it "parseRoute still reaches a matching child under the parent prefix" $
        (parseRoute (["sub"], []) :: Maybe (Route (PNoArgs ())))
            `shouldBe` Just (SubParentR SubHomeR)
