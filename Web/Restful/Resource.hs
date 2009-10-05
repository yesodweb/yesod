{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
---------------------------------------------------------
--
-- Module        : Web.Restful.Resource
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
-- Defines the Resource class.
--
---------------------------------------------------------
module Web.Restful.Resource
    ( ResourceName (..)
    , fromString
    , checkPattern
    , validatePatterns
    , checkResourceName
      -- * Testing
    , testSuite
    ) where

import Data.List.Split (splitOn)
import Web.Restful.Definitions
import Web.Restful.Handler
import Data.List (intercalate)
import Data.Enumerable
import Control.Monad (replicateM)

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck (testProperty)
import Test.HUnit hiding (Test)
import Test.QuickCheck

data ResourcePatternPiece =
    Static String
    | Dynamic String
    | Slurp String -- ^ take up the rest of the pieces. must be last
    deriving Eq
instance Show ResourcePattern where
    show = concatMap helper . unRP where
        helper (Static s) = '/' : s
        helper (Dynamic s) = '/' : '$' : s
        helper (Slurp s) = '/' : '*' : s

isSlurp :: ResourcePatternPiece -> Bool
isSlurp (Slurp _) = True
isSlurp _ = False

newtype ResourcePattern = ResourcePattern { unRP :: [ResourcePatternPiece] }
    deriving (Eq, Arbitrary)

fromString :: String -> ResourcePattern
fromString = ResourcePattern
           . map fromString' . filter (not . null) . splitOn "/"

fromString' :: String -> ResourcePatternPiece
fromString' ('$':rest) = Dynamic rest
fromString' ('*':rest) = Slurp rest
fromString' x = Static x

class (Show a, Enumerable a) => ResourceName a b | a -> b where
    -- | Get the URL pattern for each different resource name.
    -- Something like /foo/$bar/baz/ will match the regular expression
    -- /foo/(\\w*)/baz/, matching the middle part with the urlParam bar.
    --
    -- Also, /foo/*bar/ will match /foo/[anything else], capturing the value
    -- into the bar urlParam.
    resourcePattern :: a -> String

    -- | Find the handler for each resource name/verb pattern.
    getHandler :: b -> a -> Verb -> Handler

type SMap = [(String, String)]

data CheckPatternReturn = StaticMatch | DynamicMatch (String, String) | NoMatch

checkPattern :: ResourcePattern -> Resource -> Maybe SMap
checkPattern = checkPatternPieces . unRP

checkPatternPieces :: [ResourcePatternPiece] -> Resource -> Maybe SMap
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

checkPattern' :: ResourcePatternPiece -> String -> CheckPatternReturn
checkPattern' (Static x) y = if x == y then StaticMatch else NoMatch
checkPattern' (Dynamic x) y = DynamicMatch (x, y)
checkPattern' (Slurp x) _ = error $ "Slurp pattern " ++ x ++ " must be last"

combine :: SMap -> [CheckPatternReturn] -> Maybe SMap
combine s [] = Just $ reverse s
combine _ (NoMatch:_) = Nothing
combine s (StaticMatch:rest) = combine s rest
combine s (DynamicMatch x:rest) = combine (x:s) rest

overlaps :: [ResourcePatternPiece] -> [ResourcePatternPiece] -> Bool
overlaps [] [] = True
overlaps [] _ = False
overlaps _ [] = False
overlaps (Slurp _:_) _ = True
overlaps _ (Slurp _:_) = True
overlaps (Dynamic _:x) (_:y) = overlaps x y
overlaps (_:x) (Dynamic _:y) = overlaps x y
overlaps (Static a:x) (Static b:y) = a == b && overlaps x y

checkResourceName :: (Monad m, ResourceName rn model) => rn -> m ()
checkResourceName rn = do
    let avs@(y:_) = enumerate
        _ignore = asTypeOf rn y
    let patterns = map (fromString . resourcePattern) avs
    case validatePatterns patterns of
        [] -> return ()
        x -> fail $ "Overlapping patterns:\n" ++ unlines (map show x)

validatePatterns :: [ResourcePattern] -> [(ResourcePattern, ResourcePattern)]
validatePatterns [] = []
validatePatterns (x:xs) =
  concatMap (validatePatterns' x) xs ++ validatePatterns xs where
    validatePatterns' :: ResourcePattern
                   -> ResourcePattern
                   -> [(ResourcePattern, ResourcePattern)]
    validatePatterns' a b = [(a, b) | overlaps (unRP a) (unRP b)]

---- Testing
testSuite :: Test
testSuite = testGroup "Web.Restful.Resource"
    [ testCase "non-overlap" caseOverlap1
    , testCase "overlap" caseOverlap2
    , testCase "overlap-slurp" caseOverlap3
    , testCase "validatePatterns" caseValidatePatterns
    , testProperty "show pattern" prop_showPattern
    ]

caseOverlap1 :: Assertion
caseOverlap1 = assert $ not $ overlaps
                    (unRP $ fromString "/foo/$bar/")
                    (unRP $ fromString "/foo/baz/$bin")
caseOverlap2 :: Assertion
caseOverlap2 = assert $ overlaps
                    (unRP $ fromString "/foo/bar")
                    (unRP $ fromString "/foo/$baz")
caseOverlap3 :: Assertion
caseOverlap3 = assert $ overlaps
                    (unRP $ fromString "/foo/bar/baz/$bin")
                    (unRP $ fromString "*slurp")

caseValidatePatterns :: Assertion
caseValidatePatterns =
    let p1 = fromString "/foo/bar/baz"
        p2 = fromString "/foo/$bar/baz"
        p3 = fromString "/bin"
        p4 = fromString "/bin/boo"
        p5 = fromString "/bin/*slurp"
     in validatePatterns [p1, p2, p3, p4, p5] @?=
            [ (p1, p2)
            , (p4, p5)
            ]

prop_showPattern :: ResourcePattern -> Bool
prop_showPattern p = fromString (show p) == p

instance Arbitrary ResourcePatternPiece where
    arbitrary = do
        constr <- elements [Static, Dynamic, Slurp]
        size <- elements [1..10]
        s <- replicateM size $ elements ['a'..'z']
        return $ constr s
    coarbitrary = undefined
