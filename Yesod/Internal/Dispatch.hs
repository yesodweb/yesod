{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A bunch of Template Haskell used in the Yesod.Dispatch module.
module Yesod.Internal.Dispatch
    ( mkYesodDispatch'
    ) where

import Prelude hiding (exp)
import Language.Haskell.TH.Syntax
import Web.Routes.Quasi
import Web.Routes.Quasi.Parse
import Web.Routes.Quasi.TH
import Control.Monad (foldM)
import Yesod.Handler (badMethod)
import Yesod.Content (chooseRep)
import qualified Network.Wai as W
import Yesod.Core (yesodRunner, yesodDispatch)
import Data.List (foldl')
import Data.Char (toLower)
import qualified Data.ByteString.Char8 as S8
import Data.ByteString.Lazy.Char8 ()
import qualified Data.ByteString as S
import Yesod.Core (Yesod (joinPath, approot, cleanPath))
import Network.HTTP.Types (status301)
import qualified Data.Ascii as A

{-|

Alright, let's explain how routing works. We want to take a [String] and found
out which route it applies to. For static pieces, we need to ensure an exact
match against the segment. For a single or multi piece, we need to check the
result of fromSinglePiece/fromMultiPiece, respectively.

We want to create a tree of case statements basically resembling:

case testRoute1 of
    Just app -> Just app
    Nothing ->
        case testRoute2 of
            Just app -> Just app
            Nothing ->
                case testRoute3 of
                    Just app -> Just app
                    Nothing -> Nothing

Each testRoute* will look something like this (example of parsing a route /name/#String/age/#Int):

case segments of
    "name" : as ->
        case as of
            [] -> Nothing
            b:bs ->
                case fromSinglePiece b of
                    Left _ -> Nothing
                    Right name ->
                        case bs of
                            "age":cs ->
                                case cs of
                                    [] -> Nothing
                                    d:ds ->
                                        case fromSinglePiece d of
                                            Left _ -> Nothing
                                            Right age ->
                                                case ds of
                                                    [] -> Just $ yesodRunner (PersonR name age) (getPersonR name age)...
                                                    _ -> Nothing
                            _ -> Nothing
    _ -> Nothing

Obviously we would never want to write code by hand like this, but generating it is not too bad.

This function generates a clause for the yesodDispatch function based on a set of routes.

NOTE: We deal with subsites first; if none of those match, we try to apply
cleanPath. If that indicates a redirect, we perform it. Otherwise, we match
local routes.

-}

sendRedirect :: Yesod master => master -> [String] -> W.Application
sendRedirect y segments' env =
     return $ W.responseLBS status301
            [ ("Content-Type", "text/plain")
            , ("Location", A.unsafeFromString $ dest')
            ] "Redirecting"
  where
    dest = joinPath y (approot y) segments' []
    dest' =
        if S.null (W.rawQueryString env)
            then dest
            else dest ++ '?' : S8.unpack (W.rawQueryString env)

mkYesodDispatch' :: [((String, Pieces), Maybe String)]
                 -> [((String, Pieces), Maybe String)]
                 -> Q Clause
mkYesodDispatch' resSub resLoc = do
    sub <- newName "sub"
    master <- newName "master"
    mkey <- newName "mkey"
    segments <- newName "segments"
    segments' <- newName "segmentsClean"
    toMasterRoute <- newName "toMasterRoute"
    nothing <- [|Nothing|]
    bodyLoc <- foldM (go master (VarE sub) (VarE toMasterRoute) mkey segments') nothing resLoc
    cp <- [|cleanPath|]
    sr <- [|sendRedirect|]
    just <- [|Just|]
    let bodyLoc' =
            CaseE (cp `AppE` VarE master `AppE` VarE segments)
                [ Match (ConP (mkName "Left") [VarP segments'])
                        (NormalB $ just `AppE`
                           (sr `AppE` VarE master `AppE` VarE segments'))
                        []
                , Match (ConP (mkName "Right") [VarP segments'])
                        (NormalB bodyLoc)
                        []
                ]
    body <- foldM (go master (VarE sub) (VarE toMasterRoute) mkey segments) bodyLoc' resSub
    return $ Clause
        [VarP sub, VarP mkey, VarP segments, VarP master, VarP toMasterRoute]
        (NormalB body)
        []
  where
    go master sub toMasterRoute mkey segments onFail ((constr, SubSite { ssPieces = pieces }), Just toSub) = do
        test <- mkSubsiteExp segments pieces id (master, sub, toMasterRoute, mkey, constr, VarE $ mkName toSub)
        app <- newName "app"
        return $ CaseE test
            [ Match (ConP (mkName "Nothing") []) (NormalB onFail) []
            , Match (ConP (mkName "Just") [VarP app]) (NormalB $ VarE app) []
            ]
    go master sub toMasterRoute mkey segments onFail ((constr, Simple pieces methods), Nothing) = do
        test <- mkSimpleExp (VarE segments) pieces id (master, sub, toMasterRoute, mkey, constr, methods)
        just <- [|Just|]
        app <- newName "app"
        return $ CaseE test
            [ Match (ConP (mkName "Nothing") []) (NormalB onFail) []
            , Match (ConP (mkName "Just") [VarP app]) (NormalB $ just `AppE` VarE app) []
            ]
    go _ _ _ _ _ _ _ = error "Invalid combination"

mkSimpleExp :: Exp -- ^ segments
            -> [Piece]
            -> ([Exp] -> [Exp]) -- ^ variables already parsed
            -> (Name, Exp, Exp, Name, String, [String]) -- ^ master, sub, toMasterRoute, mkey, constructor, methods
            -> Q Exp
mkSimpleExp segments [] frontVars (master, sub, toMasterRoute, mkey, constr, methods) = do
    just <- [|Just|]
    nothing <- [|Nothing|]
    onSuccess <- newName "onSuccess"
    req <- newName "req"
    badMethod' <- [|badMethod|]
    rm <- [|A.toString . W.requestMethod|]
    let caseExp = rm `AppE` VarE req
    yr <- [|yesodRunner|]
    cr <- [|fmap chooseRep|]
    let url = foldl' AppE (ConE $ mkName constr) $ frontVars []
    let runHandlerVars h = runHandler' $ cr `AppE` foldl' AppE (VarE $ mkName h) (frontVars [])
        runHandler' h = NormalB $ yr `AppE` sub
                                     `AppE` VarE master
                                     `AppE` toMasterRoute
                                     `AppE` VarE mkey
                                     `AppE` (just `AppE` url)
                                     `AppE` h
                                     `AppE` VarE req
    let match m = Match (LitP $ StringL m) (runHandlerVars $ map toLower m ++ constr) []
    let clauses =
            case methods of
                [] -> [Clause [VarP req] (runHandlerVars $ "handle" ++ constr) []]
                _ -> [Clause [VarP req] (NormalB $ CaseE caseExp $ map match methods ++
                                                                   [Match WildP (runHandler' badMethod') []]) []]
    let exp = CaseE segments
                [ Match
                    (ConP (mkName "[]") [])
                    (NormalB $ just `AppE` VarE onSuccess)
                    [FunD onSuccess clauses]
                , Match
                    WildP
                    (NormalB nothing)
                    []
                ]
    return exp
mkSimpleExp segments (StaticPiece s:pieces) frontVars x = do
    srest <- newName "segments"
    innerExp <- mkSimpleExp (VarE srest) pieces frontVars x
    nothing <- [|Nothing|]
    let exp = CaseE segments
                [ Match
                    (InfixP (LitP $ StringL s) (mkName ":") (VarP srest))
                    (NormalB innerExp)
                    []
                , Match WildP (NormalB nothing) []
                ]
    return exp
mkSimpleExp segments (SinglePiece _:pieces) frontVars x = do
    srest <- newName "segments"
    next' <- newName "next'"
    innerExp <- mkSimpleExp (VarE srest) pieces (frontVars . (:) (VarE next')) x
    nothing <- [|Nothing|]
    next <- newName "next"
    fsp <- [|fromSinglePiece|]
    let exp' = CaseE (fsp `AppE` VarE next)
                [ Match
                    (ConP (mkName "Left") [WildP])
                    (NormalB nothing)
                    []
                , Match
                    (ConP (mkName "Right") [VarP next'])
                    (NormalB innerExp)
                    []
                ]
    let exp = CaseE segments
                [ Match
                    (InfixP (VarP next) (mkName ":") (VarP srest))
                    (NormalB exp')
                    []
                , Match WildP (NormalB nothing) []
                ]
    return exp
mkSimpleExp segments [MultiPiece _] frontVars x = do
    next' <- newName "next'"
    srest <- [|[]|]
    innerExp <- mkSimpleExp srest [] (frontVars . (:) (VarE next')) x
    nothing <- [|Nothing|]
    fmp <- [|fromMultiPiece|]
    let exp = CaseE (fmp `AppE` segments)
                [ Match
                    (ConP (mkName "Left") [WildP])
                    (NormalB nothing)
                    []
                , Match
                    (ConP (mkName "Right") [VarP next'])
                    (NormalB innerExp)
                    []
                ]
    return exp
mkSimpleExp _ (MultiPiece _:_) _ _ = error "MultiPiece must be last piece"

mkSubsiteExp :: Name -- ^ segments
             -> [Piece]
             -> ([Exp] -> [Exp]) -- ^ variables already parsed
             -> (Name, Exp, Exp, Name, String, Exp) -- ^ master, sub, toMasterRoute, mkey, constructor, toSub
             -> Q Exp
mkSubsiteExp segments [] frontVars (master, sub, toMasterRoute, mkey, constr, toSub) = do
    yd <- [|yesodDispatch|]
    dot <- [|(.)|]
    let con = InfixE (Just toMasterRoute) dot $ Just $ foldl' AppE (ConE $ mkName constr) $ frontVars []
    -- proper handling for sub-subsites
    let sub' = foldl' AppE (toSub `AppE` sub) $ frontVars []
    let app = yd `AppE` sub'
                 `AppE` VarE mkey
                 `AppE` VarE segments
                 `AppE` VarE master
                 `AppE` con
    just <- [|Just|]
    return $ just `AppE` app
mkSubsiteExp _ (MultiPiece _:_) _ _ = error "Subsites cannot have MultiPiece"
mkSubsiteExp segments (StaticPiece s:pieces) frontVars x = do
    srest <- newName "segments"
    innerExp <- mkSubsiteExp srest pieces frontVars x
    nothing <- [|Nothing|]
    let exp = CaseE (VarE segments)
                [ Match
                    (InfixP (LitP $ StringL s) (mkName ":") (VarP srest))
                    (NormalB innerExp)
                    []
                , Match WildP (NormalB nothing) []
                ]
    return exp
mkSubsiteExp segments (SinglePiece _:pieces) frontVars x = do
    srest <- newName "segments"
    next' <- newName "next'"
    innerExp <- mkSubsiteExp srest pieces (frontVars . (:) (VarE next')) x
    nothing <- [|Nothing|]
    next <- newName "next"
    fsp <- [|fromSinglePiece|]
    let exp' = CaseE (fsp `AppE` VarE next)
                [ Match
                    (ConP (mkName "Left") [WildP])
                    (NormalB nothing)
                    []
                , Match
                    (ConP (mkName "Right") [VarP next'])
                    (NormalB innerExp)
                    []
                ]
    let exp = CaseE (VarE segments)
                [ Match
                    (InfixP (VarP next) (mkName ":") (VarP srest))
                    (NormalB exp')
                    []
                , Match WildP (NormalB nothing) []
                ]
    return exp
