{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
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
    , checkPatternsTH
    , validatePatterns
    , checkPatterns
    , checkRPNodes
    , rpnodesTH
    , rpnodesTHCheck
    , rpnodesQuasi
    , RPNode (..)
    , VerbMap (..)
    , RP (..)
    , RPP (..)
#if TEST
      -- * Testing
    , testSuite
#endif
    ) where

import Data.List.Split (splitOn)
import Yesod.Definitions
import Data.List (intercalate, nub)
import Data.Char (isDigit)

import Control.Monad (when)
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote

import Data.Typeable (Typeable)
import Control.Exception (Exception)
import Data.Attempt -- for failure stuff
import Data.Object.Text
import Control.Monad ((<=<))
import Data.Object.Yaml

#if TEST
import Control.Monad (replicateM)
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
    deriving (Eq, Show)

-- | Resource Pattern
newtype RP = RP { unRP :: [RPP] }
    deriving (Eq, Show)

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
instance ConvertSuccess RP String where
    convertSuccess = concatMap helper . unRP where
        helper (Static s) = '/' : s
        helper (Dynamic s) = '/' : '$' : s
        helper (Slurp s) = '/' : '*' : s
        helper (DynInt s) = '/' : '#' : s

type ResourcePattern = String

type SMap = [(String, String)]

data CheckPatternReturn =
    StaticMatch
  | DynamicMatch (String, String)
  | NoMatch

checkPattern :: RP -> Resource -> Maybe SMap
checkPattern = checkPatternPieces . unRP

checkPatternsTH :: Bool -> [ResourcePattern] -> Q Exp
checkPatternsTH toCheck patterns = do
    runIO $ when toCheck $ checkPatterns patterns
    [|return ()|]

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

checkPatterns :: MonadFailure OverlappingPatterns f
              => [ResourcePattern]
              -> f ()
checkPatterns patterns =
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

data RPNode = RPNode RP VerbMap
    deriving (Show, Eq)
data VerbMap = AllVerbs String | Verbs [(Verb, String)]
    deriving (Show, Eq)
instance ConvertAttempt YamlDoc [RPNode] where
    convertAttempt = fromTextObject <=< ca
instance ConvertAttempt TextObject [RPNode] where
    convertAttempt = mapM helper <=< fromMapping where
        helper :: (Text, TextObject) -> Attempt RPNode
        helper (rp, rest) = do
            verbMap <- fromTextObject rest
            let rp' = cs (cs rp :: String)
            return $ RPNode rp' verbMap
instance ConvertAttempt TextObject VerbMap where
    convertAttempt (Scalar s) = return $ AllVerbs $ cs s
    convertAttempt (Mapping m) = Verbs `fmap` mapM helper m where
        helper :: (Text, TextObject) -> Attempt (Verb, String)
        helper (v, Scalar f) = do
            v' <- ca (cs v :: String)
            return (v', cs f)
        helper (_, x) = failure $ VerbMapNonScalar x
    convertAttempt o = failure $ VerbMapSequence o
data RPNodeException = VerbMapNonScalar TextObject
                     | VerbMapSequence TextObject
    deriving (Show, Typeable)
instance Exception RPNodeException

checkRPNodes :: (MonadFailure OverlappingPatterns m,
                 MonadFailure RepeatedVerb m
                )
             => [RPNode]
             -> m [RPNode]
checkRPNodes nodes = do
    checkPatterns $ map (\(RPNode r _) -> cs r) nodes -- FIXME ugly
    mapM_ (\(RPNode _ v) -> checkVerbMap v) nodes
    return nodes
        where
            checkVerbMap (AllVerbs _) = return ()
            checkVerbMap (Verbs vs) =
                let vs' = map fst vs
                    res = nub vs' == vs'
                 in if res then return () else failure $ RepeatedVerb vs

newtype RepeatedVerb = RepeatedVerb [(Verb, String)]
    deriving (Show, Typeable)
instance Exception RepeatedVerb

rpnodesTHCheck :: [RPNode] -> Q Exp
rpnodesTHCheck nodes = do
    nodes' <- runIO $ checkRPNodes nodes
    rpnodesTH nodes'

rpnodesTH :: [RPNode] -> Q Exp
rpnodesTH = fmap ListE . mapM lift
instance Lift RPNode where
    lift (RPNode rp vm) = do
        rp' <- lift rp
        vm' <- lift vm
        return $ TupE [rp', vm']
instance Lift RP where
    lift (RP rpps) = do
        rpps' <- lift rpps
        return $ ConE (mkName "RP") `AppE` rpps'
instance Lift RPP where
    lift (Static s) =
        return $ ConE (mkName "Static") `AppE` (LitE $ StringL s)
    lift (Dynamic s) =
        return $ ConE (mkName "Dynamic") `AppE` (LitE $ StringL s)
    lift (DynInt s) =
        return $ ConE (mkName "DynInt") `AppE` (LitE $ StringL s)
    lift (Slurp s) =
        return $ ConE (mkName "Slurp") `AppE` (LitE $ StringL s)
instance Lift VerbMap where
    lift (AllVerbs s) = return $ ListE $ map helper [minBound..maxBound]
        where
            helper :: Verb -> Exp
            helper v = TupE [(helper2 v), LitE $ StringL s]
            helper2 :: Verb -> Exp
            helper2 = ConE . mkName . show
    lift (Verbs v) = lift v

strToExp :: String -> Q Exp
strToExp s = do
    let yd :: YamlDoc
        yd = YamlDoc $ cs s
    rpnodes <- runIO $ convertAttemptWrap yd
    rpnodesTHCheck rpnodes

rpnodesQuasi :: QuasiQuoter
rpnodesQuasi = QuasiQuoter strToExp undefined

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
    , testCase "read patterns from YAML" caseFromYaml
    , testCase "checkRPNodes" caseCheckRPNodes
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
prop_showPattern p = cs (cs p :: String) == p

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

caseFromYaml :: Assertion
caseFromYaml = do
    contents <- readYamlDoc "test/resource-patterns.yaml"
    let expected =
         [ RPNode (cs "static/*filepath") $ AllVerbs "getStatic"
         , RPNode (cs "page") $ Verbs [(Get, "pageIndex"), (Put, "pageAdd")]
         , RPNode (cs "page/$page") $ Verbs [ (Get, "pageDetail")
                                            , (Delete, "pageDelete")
                                            , (Post, "pageUpdate")
                                            ]
         , RPNode (cs "user/#id") $ Verbs [(Get, "userInfo")]
         ]
    contents' <- fa $ ca contents
    expected @=? contents'

caseCheckRPNodes :: Assertion
caseCheckRPNodes = do
    good' <- readYamlDoc "test/resource-patterns.yaml"
    good <- fa $ ca good'
    Just good @=? checkRPNodes good
    let bad1 = [ RPNode (cs "foo/bar") $ AllVerbs "foo"
               , RPNode (cs "$foo/bar") $ AllVerbs "bar"
               ]
    Nothing @=? checkRPNodes bad1
    let bad2 = [RPNode (cs "") $ Verbs [(Get, "foo"), (Get, "bar")]]
    Nothing @=? checkRPNodes bad2
#endif
