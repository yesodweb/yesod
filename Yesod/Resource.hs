{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
---------------------------------------------------------
--
-- Module        : Yesod.Resource
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
-- Defines the ResourceName class.
--
---------------------------------------------------------
module Yesod.Resource
    ( ResourcePattern
    , checkPattern
    , validatePatterns
    , checkResourceName
#if TEST
      -- * Testing
    , testSuite
#endif
    ) where

import Data.List.Split (splitOn)
import Yesod.Definitions
import Data.List (intercalate)
import Data.Char (isDigit)

import Data.Typeable (Typeable)
import Control.Exception (Exception)
import Data.Attempt -- for failure stuff
import Data.Convertible.Text

#if TEST
import Control.Monad (replicateM, when)
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck (testProperty)
import Test.HUnit hiding (Test)
import Test.QuickCheck
#endif

-- | Resource Pattern Piece
data RPP =
    Static String
    | Dynamic String
    | DynInt String
    | Slurp String -- ^ take up the rest of the pieces. must be last
    deriving Eq

-- | Resource Pattern
newtype RP = RP { unRP :: [RPP] }
    deriving Eq

instance Show RP where
    show = concatMap helper . unRP where
        helper (Static s) = '/' : s
        helper (Dynamic s) = '/' : '$' : s
        helper (Slurp s) = '/' : '*' : s
        helper (DynInt s) = '/' : '#' : s

isSlurp :: RPP -> Bool
isSlurp (Slurp _) = True
isSlurp _ = False

instance ConvertSuccess String RP where
    convertSuccess = RP . map helper . filter (not . null) .splitOn "/"
      where
        helper :: String -> RPP
        helper ('$':rest) = Dynamic rest
        helper ('*':rest) = Slurp rest
        helper ('#':rest) = DynInt rest
        helper x = Static x

type ResourcePattern = String

type SMap = [(String, String)]

data CheckPatternReturn =
    StaticMatch
  | DynamicMatch (String, String)
  | NoMatch

checkPattern :: RP -> Resource -> Maybe SMap
checkPattern = checkPatternPieces . unRP

checkPatternPieces :: [RPP] -> Resource -> Maybe SMap
checkPatternPieces rp r
    | not (null rp) && isSlurp (last rp) = do
        let rp' = init rp
            (r1, r2) = splitAt (length rp') r
        smap <- checkPatternPieces rp' r1
        let slurpValue = intercalate "/" r2
            Slurp slurpKey = last rp
        return $ (slurpKey, slurpValue) : smap
    | length rp /= length r = Nothing
    | otherwise = combine [] $ zipWith checkPattern' rp r

checkPattern' :: RPP -> String -> CheckPatternReturn
checkPattern' (Static x) y = if x == y then StaticMatch else NoMatch
checkPattern' (Dynamic x) y = DynamicMatch (x, y)
checkPattern' (Slurp x) _ = error $ "Slurp pattern " ++ x ++ " must be last"
checkPattern' (DynInt x) y
    | all isDigit y = DynamicMatch (x, y)
    | otherwise = NoMatch

combine :: SMap -> [CheckPatternReturn] -> Maybe SMap
combine s [] = Just $ reverse s
combine _ (NoMatch:_) = Nothing
combine s (StaticMatch:rest) = combine s rest
combine s (DynamicMatch x:rest) = combine (x:s) rest

overlaps :: [RPP] -> [RPP] -> Bool
overlaps [] [] = True
overlaps [] _ = False
overlaps _ [] = False
overlaps (Slurp _:_) _ = True
overlaps _ (Slurp _:_) = True
overlaps (Dynamic _:x) (_:y) = overlaps x y
overlaps (_:x) (Dynamic _:y) = overlaps x y
overlaps (DynInt _:x) (DynInt _:y) = overlaps x y
overlaps (DynInt _:x) (Static s:y)
    | all isDigit s = overlaps x y
    | otherwise = False
overlaps (Static s:x) (DynInt _:y)
    | all isDigit s = overlaps x y
    | otherwise = False
overlaps (Static a:x) (Static b:y) = a == b && overlaps x y

data OverlappingPatterns =
    OverlappingPatterns [(ResourcePattern, ResourcePattern)]
    deriving (Show, Typeable)
instance Exception OverlappingPatterns

checkResourceName :: MonadFailure OverlappingPatterns f
                  => [ResourcePattern]
                  -> f ()
checkResourceName patterns =
    case validatePatterns patterns of
        [] -> return ()
        x -> failure $ OverlappingPatterns x

validatePatterns :: [ResourcePattern]
                 -> [(ResourcePattern, ResourcePattern)]
validatePatterns [] = []
validatePatterns (x:xs) =
  concatMap (validatePatterns' x) xs ++ validatePatterns xs where
    validatePatterns' :: ResourcePattern
                      -> ResourcePattern
                      -> [(ResourcePattern, ResourcePattern)]
    validatePatterns' a b =
        let a' = unRP $ cs a
            b' = unRP $ cs b
         in [(a, b) | overlaps a' b']

#if TEST
---- Testing
testSuite :: Test
testSuite = testGroup "Yesod.Resource"
    [ testCase "non-overlap" caseOverlap1
    , testCase "overlap" caseOverlap2
    , testCase "overlap-slurp" caseOverlap3
    , testCase "validatePatterns" caseValidatePatterns
    , testProperty "show pattern" prop_showPattern
    , testCase "integers" caseIntegers
    ]

deriving instance Arbitrary RP

caseOverlap1 :: Assertion
caseOverlap1 = assert $ not $ overlaps
                    (unRP $ cs "/foo/$bar/")
                    (unRP $ cs "/foo/baz/$bin")
caseOverlap2 :: Assertion
caseOverlap2 = assert $ overlaps
                    (unRP $ cs "/foo/bar")
                    (unRP $ cs "/foo/$baz")
caseOverlap3 :: Assertion
caseOverlap3 = assert $ overlaps
                    (unRP $ cs "/foo/bar/baz/$bin")
                    (unRP $ cs "*slurp")

caseValidatePatterns :: Assertion
caseValidatePatterns =
    let p1 = cs "/foo/bar/baz"
        p2 = cs "/foo/$bar/baz"
        p3 = cs "/bin"
        p4 = cs "/bin/boo"
        p5 = cs "/bin/*slurp"
     in validatePatterns [p1, p2, p3, p4, p5] @?=
            [ (p1, p2)
            , (p4, p5)
            ]

prop_showPattern :: RP -> Bool
prop_showPattern p = cs (show p) == p

caseIntegers :: Assertion
caseIntegers = do
    let p1 = "/foo/#bar/"
        p2 = "/foo/#baz/"
        p3 = "/foo/$bin/"
        p4 = "/foo/4/"
        p5 = "/foo/bar/"
        p6 = "/foo/*slurp/"
        checkOverlap :: String -> String -> Bool -> IO ()
        checkOverlap a b c = do
            let res1 = overlaps (unRP $ cs a) (unRP $ cs b)
            let res2 = overlaps (unRP $ cs b) (unRP $ cs a)
            when (res1 /= c || res2 /= c) $ assertString $ a
               ++ (if c then " does not overlap with " else " overlaps with ")
               ++ b
    checkOverlap p1 p2 True
    checkOverlap p1 p3 True
    checkOverlap p1 p4 True
    checkOverlap p1 p5 False
    checkOverlap p1 p6 True

instance Arbitrary RPP where
    arbitrary = do
        constr <- elements [Static, Dynamic, Slurp, DynInt]
        size <- elements [1..10]
        s <- replicateM size $ elements ['a'..'z']
        return $ constr s
    coarbitrary = undefined
#endif
