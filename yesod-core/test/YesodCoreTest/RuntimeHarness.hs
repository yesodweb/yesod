{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A single WAI round-trip helper shared by the parameterized / nested
-- dispatch runtime specs. Previously each of those modules carried its own
-- near-identical @testRequestIO@, in two disagreeing styles
-- (@shouldBe@-on-@simpleStatus@ vs. @assertStatus@). This is the one shared
-- harness: it takes the action that builds the WAI app plus the request
-- details, and asserts the response status (and body, when given) using
-- 'assertStatus' / 'assertBody' so a mismatch prints the offending response.
module YesodCoreTest.RuntimeHarness
    ( assertRequest
    , assertRequestFor
    , assertGet
    , assertRequestRaw
    ) where

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Network.Wai (Application, Request, pathInfo, requestMethod)
import Network.Wai.Test
import Test.Hspec (HasCallStack)
import Yesod.Core (Yesod, YesodDispatch, toWaiApp)
import qualified Network.HTTP.Types as H

-- | Build the WAI app, issue one request at @path@ with @method@, assert the
-- response status, and (when given) assert the body.
assertRequest
    :: HasCallStack
    => IO Application      -- ^ build the WAI app (e.g. @toWaiApp App@)
    -> H.Method            -- ^ request method
    -> Int                 -- ^ expected status code
    -> [Text]              -- ^ request path
    -> Maybe ByteString    -- ^ expected body, if checked
    -> IO ()
assertRequest mkApp method status path mexpected = do
    app <- mkApp
    flip runSession app $ do
        sres <- request defaultRequest
            { pathInfo = path
            , requestMethod = method
            }
        assertStatus status sres
        case mexpected of
            Nothing -> pure ()
            Just expected -> assertBody expected sres

-- | 'assertRequest' specialised to a concrete site: builds the WAI app with
-- 'toWaiApp' for you. This is the shape almost every runtime spec wants, and
-- standardises the argument order (@site → method → status → path → body@) that
-- the per-module @testRequestIO@ wrappers used to spell inconsistently.
assertRequestFor
    :: (HasCallStack, Yesod site, YesodDispatch site)
    => site                -- ^ the site to dispatch against
    -> H.Method            -- ^ request method
    -> Int                 -- ^ expected status code
    -> [Text]              -- ^ request path
    -> Maybe ByteString    -- ^ expected body, if checked
    -> IO ()
assertRequestFor site = assertRequest (toWaiApp site)

-- | 'assertRequestFor' fixed to @GET@ — the common case.
assertGet
    :: (HasCallStack, Yesod site, YesodDispatch site)
    => site -> Int -> [Text] -> Maybe ByteString -> IO ()
assertGet site = assertRequestFor site "GET"

-- | Like 'assertRequest', but issue a fully caller-built 'Request' (e.g. one
-- carrying an @Accept@ header for content negotiation) rather than just a
-- path + method. 'assertStatus'/'assertBody' already print the offending
-- response on a mismatch, so callers don't need their own annotate-on-failure
-- wrapper.
assertRequestRaw
    :: HasCallStack
    => IO Application -> Request -> Int -> Maybe ByteString -> IO ()
assertRequestRaw mkApp req status mexpected = do
    app <- mkApp
    flip runSession app $ do
        sres <- request req
        assertStatus status sres
        case mexpected of
            Nothing -> pure ()
            Just expected -> assertBody expected sres
