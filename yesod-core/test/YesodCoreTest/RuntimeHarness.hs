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
    ) where

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Network.Wai (Application, pathInfo, requestMethod)
import Network.Wai.Test
import Test.Hspec (HasCallStack)
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
