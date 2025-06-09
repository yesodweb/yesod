{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module GeneratorTestUtil where

import Control.Monad (when)
import Data.List (sortBy)
import Language.Haskell.TH
import Test.HUnit
import Yesod.EmbeddedStatic.Types as Y
import qualified Data.ByteString.Lazy as BL
import RIO (HasCallStack)

-- We test the generators by executing them at compile time
-- and sticking the result into the GenTestResult.  We then
-- test the GenTestResult at runtime.  But to test the ebDevelReload
-- we must run the action at runtime so that is also embedded.
-- Because of template haskell stage restrictions, this code
-- needs to be in a separate module.

data GenTestResult = GenError String
                   | GenSuccessWithDevel (IO BL.ByteString)

-- | Creates a GenTestResult at compile time by testing the entry.
testEntry :: Maybe String -> Y.Location -> IO BL.ByteString -> Entry -> ExpQ
testEntry name _ _ e | ebHaskellName e /= (mkName <$> name) =
    [| GenError ("haskell name " ++ $(litE $ stringL $ show $ ebHaskellName e)
                                 ++ " /= "
                                 ++ $(litE $ stringL $ show name)) |]
testEntry _ loc _ e  | ebLocation e /= loc =
    [| GenError ("location " ++ $(litE $ stringL $ show $ ebLocation e)) |]
testEntry _ _ act e = do
    expected <- fmap stripCR $ runIO act
    actual <- fmap stripCR $ runIO $ ebProductionContent e
    if expected == actual
        then [| GenSuccessWithDevel $(ebDevelReload e) |]
        else [| GenError $ "production content: " ++ $(litE $ stringL $ show (expected, actual)) |]

-- | Remove all carriage returns, for Windows testing
stripCR :: BL.ByteString -> BL.ByteString
stripCR = BL.filter (/= 13)

testOneEntry :: Maybe String -> Y.Location -> IO BL.ByteString -> [Entry] -> ExpQ
testOneEntry name loc ct [e] = testEntry name loc ct e
testOneEntry _ _ _ _ = [| GenError "not exactly one entry" |]

-- | Tests a list of entries
testEntries :: [(Maybe String, Y.Location, IO BL.ByteString)] -> [Entry] -> ExpQ
testEntries a b | length a /= length b = [| [GenError "lengths differ"] |]
testEntries a b = listE $ zipWith f a' b'
    where
        a' = sortBy (\(_,l1,_) (_,l2,_) -> compare l1 l2) a
        b' = sortBy (\e1 e2 -> ebLocation e1 `compare` ebLocation e2) b
        f (name, loc, ct) e = testEntry name loc ct e

-- | Use this at runtime to assert the 'GenTestResult' is OK
assertGenResult :: HasCallStack
                => (IO BL.ByteString) -- ^ expected development content
                -> GenTestResult -- ^ test result created at compile time
                -> Assertion
assertGenResult _ (GenError e) = assertFailure ("invalid " ++ e)
assertGenResult mexpected (GenSuccessWithDevel mactual) = do
    expected <- fmap stripCR mexpected
    actual <- fmap stripCR mactual
    when (expected /= actual) $
        assertFailure $ "invalid devel content: " ++ show (expected, actual)
