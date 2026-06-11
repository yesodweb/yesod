{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | T2: a @!@-separated dynamic route (@\/!#Int@) is only ever exercised by the
-- render/parse specs (in the @test-routes@ suite's pure-routing @Hierarchy@
-- fixture). The @!@ flips /compile-time/ overlap checking only, so at runtime
-- @\/!#Int@ dispatches exactly like @\/#Int@. This pins that equivalence with a
-- real WAI round-trip.
module YesodCoreTest.BangSeparatorRuntime
    ( specs
    ) where

import Test.Hspec
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Yesod.Core

import YesodCoreTest.RuntimeHarness (assertGet)

data BangApp = BangApp

mkYesod "BangApp" [parseRoutes|
/!#Int BackwardsR GET
/plain/#Int PlainR GET
|]

instance Yesod BangApp where
    messageLoggerSource = mempty

getBackwardsR :: Int -> HandlerFor BangApp Text
getBackwardsR i = pure (toPathPiece i <> ":backwards")

getPlainR :: Int -> HandlerFor BangApp Text
getPlainR i = pure (toPathPiece i <> ":plain")

req :: HasCallStack => Int -> [Text] -> Maybe ByteString -> IO ()
req = assertGet BangApp

specs :: Spec
specs = describe "!-separator dynamic route dispatch (/!#Int)" $ do
    it "dispatches BackwardsR on a numeric piece, like a plain /#Int" $
        req 200 ["5"] (Just "5:backwards")
    it "still 404s a non-numeric piece (the # parser is unchanged by !)" $
        req 404 ["notanint"] Nothing
    it "dispatches the plain /#Int sibling for contrast" $
        req 200 ["plain", "7"] (Just "7:plain")
