{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Yesod.Formable
    ( Form (..)
    , Formlet
    , FormResult (..)
    , runForm
    , incr
    , Formable (..)
    , deriveFormable
    , share2
    , wrapperRow
    , sealFormlet
    , sealForm
    , NonEmptyString (..)
    , Slug (..)
    ) where

import Text.Hamlet
import Data.Time (Day)
import Control.Applicative
import Database.Persist (Persistable, Table (..))
import Data.Char (isAlphaNum, toUpper, isUpper)
import Language.Haskell.TH.Syntax
import Control.Monad (liftM)
import Control.Arrow (first)
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid (mempty, mappend)
import qualified Data.ByteString.Lazy.UTF8
import Yesod.Request
import Yesod.Handler
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State
import Web.Routes.Quasi (Routes, SinglePiece)

runForm :: Form sub y a
        -> GHandler sub y (FormResult a, Hamlet (Routes y))
runForm f = do
    req <- getRequest
    (pp, _) <- liftIO $ reqRequestBody req
    evalStateT (deform f pp) 1

type Env = [(String, String)]

type Incr = StateT Int

incr :: Monad m => Incr m Int
incr = do
    i <- get
    let i' = i + 1
    put i'
    return i'

data FormResult a = FormMissing
                  | FormFailure [String]
                  | FormSuccess a
instance Functor FormResult where
    fmap _ FormMissing = FormMissing
    fmap _ (FormFailure errs) = FormFailure errs
    fmap f (FormSuccess a) = FormSuccess $ f a
instance Applicative FormResult where
    pure = FormSuccess
    (FormSuccess f) <*> (FormSuccess g) = FormSuccess $ f g
    (FormFailure x) <*> (FormFailure y) = FormFailure $ x ++ y
    (FormFailure x) <*> _ = FormFailure x
    _ <*> (FormFailure y) = FormFailure y
    _ <*> _ = FormMissing

newtype Form sub y a = Form
    { deform :: Env -> Incr (GHandler sub y) (FormResult a, Hamlet (Routes y))
    }
type Formlet sub y a = Maybe a -> Form sub y a

instance Functor (Form sub url) where
    fmap f (Form g) = Form $ \env -> liftM (first $ fmap f) (g env)

instance Applicative (Form sub url) where
    pure a = Form $ const $ return (pure a, mempty)
    (Form f) <*> (Form g) = Form $ \env -> do
        (f1, f2) <- f env
        (g1, g2) <- g env
        return (f1 <*> g1, f2 `mappend` g2)

sealForm :: ([String] -> Hamlet (Routes y) -> Hamlet (Routes y))
         -> Form sub y a -> Form sub y a
sealForm wrapper (Form form) = Form $ \env -> liftM go (form env)
  where
    go (res, xml) = (res, wrapper (toList res) xml)
    toList (FormFailure errs) = errs
    toList _ = []

sealFormlet :: ([String] -> Hamlet (Routes y) -> Hamlet (Routes y))
            -> Formlet sub y a -> Formlet sub y a
sealFormlet wrapper formlet initVal = sealForm wrapper $ formlet initVal

input' :: (String -> String -> Hamlet (Routes y))
       -> Maybe String
       -> Form sub y String
input' mkXml val = Form $ \env -> do
    i <- incr
    let i' = show i
    let param = lookup i' env
    let xml = mkXml i' $ fromMaybe (fromMaybe "" val) param
    return (maybe FormMissing FormSuccess param, xml)

check :: Form sub url a -> (a -> Either [String] b) -> Form sub url b
check (Form form) f = Form $ \env -> liftM (first go) (form env)
  where
    go FormMissing = FormMissing
    go (FormFailure x) = FormFailure x
    go (FormSuccess a) =
        case f a of
            Left errs -> FormFailure errs
            Right b -> FormSuccess b

class Formable a where
    formable :: Formlet sub master a

wrapperRow :: String -> [String] -> Hamlet url -> Hamlet url
wrapperRow label errs control = [$hamlet|
%tr
    %th $string.label$
    %td ^control^
    $if not.null.errs
        %td.errors
            %ul
                $forall errs err
                    %li $string.err$
|]

instance Formable String where
    formable = input' go
      where
        go name val = [$hamlet|
%input!type=text!name=$string.name$!value=$string.val$
|]

instance Formable Html where
    formable = fmap preEscapedString
              . input' go
              . fmap (Data.ByteString.Lazy.UTF8.toString . renderHtml)
      where
        go name val = [$hamlet|%textarea!name=$string.name$ $string.val$|]

instance Formable Day where
    formable x = input' go (fmap show x) `check` asDay
      where
        go name val = [$hamlet|
%input!type=date!name=$string.name$!value=$string.val$
|]
        asDay s = case reads s of
                    (y, _):_ -> Right y
                    [] -> Left ["Invalid day"]

instance Formable Bool where
    formable x = Form $ \env -> do
        i <- incr
        let i' = show i
        let param = lookup i' env
        let def = if null env then fromMaybe False x else isJust param
        return (FormSuccess $ isJust param, go i' def)
      where
        go name val = [$hamlet|
%input!type=checkbox!name=$string.name$!:val:checked
|]

instance Formable Int where
    formable x = input' go (fmap show x) `check` asInt
      where
        go name val = [$hamlet|
%input!type=number!name=$string.name$!value=$string.val$
|]
        asInt s = case reads s of
                    (y, _):_ -> Right y
                    [] -> Left ["Invalid integer"]

newtype Slug = Slug { unSlug :: String }
    deriving (Read, Eq, Show, SinglePiece, Persistable)

instance Formable Slug where
    formable x = input' go (fmap unSlug x) `check` asSlug
      where
        go name val = [$hamlet|
%input!type=text!name=$string.name$!value=$string.val$
|]
        asSlug [] = Left ["Slug must be non-empty"]
        asSlug x'
            | all (\c -> c `elem` "-_" || isAlphaNum c) x' =
                Right $ Slug x'
            | otherwise = Left ["Slug must be alphanumeric, - and _"]

newtype NonEmptyString = NonEmptyString { unNonEmptyString :: String }
    deriving (Read, Eq, Show, SinglePiece, Persistable)
instance Formable NonEmptyString where
    formable x = input' go (fmap unNonEmptyString x) `check` notEmpty
      where
        go name val = [$hamlet|
%input!type=text!name=$string.name$!value=$string.val$
|]
        notEmpty "" = Left ["Must be non-empty"]
        notEmpty y = Right $ NonEmptyString y

share2 :: Monad m => (a -> m [b]) -> (a -> m [b]) -> a -> m [b]
share2 f g a = do
    f' <- f a
    g' <- g a
    return $ f' ++ g'

deriveFormable :: [Table] -> Q [Dec]
deriveFormable = mapM derive
  where
    derive :: Table -> Q Dec
    derive t = do
        let cols = map (toLabel . fst) $ tableColumns t
        ap <- [|(<*>)|]
        just <- [|pure|]
        nothing <- [|Nothing|]
        let just' = just `AppE` ConE (mkName $ tableName t)
        let c1 = Clause [ ConP (mkName "Nothing") []
                        ]
                        (NormalB $ go ap just' $ zip cols $ map (const nothing) cols)
                        []
        xs <- mapM (const $ newName "x") cols
        let xs' = map (AppE just . VarE) xs
        let c2 = Clause [ ConP (mkName "Just") [ConP (mkName $ tableName t)
                            $ map VarP xs]]
                        (NormalB $ go ap just' $ zip cols xs')
                        []
        return $ InstanceD [] (ConT ''Formable
                              `AppT` ConT (mkName $ tableName t))
            [FunD (mkName "formable") [c1, c2]]
    go ap just' = foldl (ap' ap) just' . map go'
    go' (label, ex) =
        VarE (mkName "sealForm") `AppE`
        (VarE (mkName "wrapperRow") `AppE` LitE (StringL label)) `AppE`
        (VarE (mkName "formable") `AppE` ex)
    ap' ap x y = InfixE (Just x) ap (Just y)

toLabel :: String -> String
toLabel "" = ""
toLabel (x:rest) = toUpper x : go rest
  where
    go "" = ""
    go (c:cs)
        | isUpper c = ' ' : c : go cs
        | otherwise = c : go cs
