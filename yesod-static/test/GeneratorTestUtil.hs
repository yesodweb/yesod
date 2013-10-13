{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module GeneratorTestUtil where

import Control.Applicative
import Control.Monad (when)
import Data.List (sortBy)
import Language.Haskell.TH
import Test.HUnit
import Yesod.EmbeddedStatic.Types
import qualified Data.ByteString.Lazy as BL

-- We test the generators by executing them at compile time
-- and sticking the result into the GenTestResult.  We then
-- test the GenTestResult at runtime.  But to test the ebDevelReload
-- we must run the action at runtime so that is also embedded.
-- Because of template haskell stage restrictions, this code
-- needs to be in a separate module.

data GenTestResult = GenError String
                   | GenSuccessWithDevel (IO BL.ByteString)

-- | Creates a GenTestResult at compile time by testing the entry.
testEntry :: Entry -> Maybe String -> Location -> IO BL.ByteString -> ExpQ
testEntry  e name loc act 
    | ebHaskellName e /= (mkName <$> name) =
        [| GenError ("haskell name " ++ $(litE $ stringL $ show $ ebHaskellName e)
                                     ++ " /= "
                                     ++ $(litE $ stringL $ show name)) |]

    | ebLocation e /= loc =
        [| GenError ("location " ++ $(litE $ stringL $ show $ ebLocation e)) |]

    | otherwise = do
        expected <- runIO act
        actual <- runIO $ ebProductionContent e
        if expected == actual
            then [| GenSuccessWithDevel $(ebDevelReload e) |]
            else [| GenError "production content" |]

-- | Tests a list of entries 
testEntries :: [Entry] -> [(Maybe String, Location, IO BL.ByteString)] -> ExpQ
testEntries a b | length a /= length b = [| [GenError "lengths differ"] |]
testEntries a b = listE $ zipWith f a' b'
    where 
        a' = sortBy (\e1 e2 -> ebLocation e1 `compare` ebLocation e2) a
        b' = sortBy (\(_,l1,_) (_,l2,_) -> compare l1 l2) b
        f e (name, loc, ct) = testEntry e name loc ct

-- | Use this at runtime to assert the 'GenTestResult' is OK
assertGenResult :: (IO BL.ByteString) -- ^ expected development content
                -> GenTestResult -- ^ test result created at compile time
                -> Assertion
assertGenResult _ (GenError e) = assertFailure ("invalid " ++ e)
assertGenResult mexpected (GenSuccessWithDevel mactual) = do
    expected <- mexpected
    actual <- mactual
    when (expected /= actual) $
        assertFailure "invalid devel content"
