{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Yesod.Contrib.Formable where

import Text.Hamlet
import Data.Time (Day)
import Control.Applicative
import Web.Routes.Quasi (SinglePiece)
import Database.Persist (Persistable)
import Data.Char (isAlphaNum)
import Language.Haskell.TH.Syntax
import Database.Persist (Table (..))
import Database.Persist.Helper (upperFirst)
import Control.Monad (liftM)
import Control.Arrow (first)
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty, mappend)
import qualified Data.ByteString.Lazy.UTF8

type Env = [(String, String)]

newtype Incr a = Incr { runIncr :: Int -> (a, Int) }
incr :: Incr Int
incr = Incr $ \i -> (i + 1, i + 1)
instance Monad Incr where
    return a = Incr $ \i -> (a, i)
    Incr x >>= f = Incr $ \i ->
        let (x', i') = x i
         in runIncr (f x') i'

data FormResult a = FormMissing
                  | FormFailure [String]
                  | FormSuccess a
instance Functor FormResult where
    fmap _ FormMissing = FormMissing
    fmap _ (FormFailure errs) = FormFailure errs
    fmap f (FormSuccess a) = FormSuccess $ f a

newtype Form url a = Form
    { runForm :: Env -> Incr (FormResult a, Hamlet url)
    }
type Formlet url a = Maybe a -> Form url a

newtype SealedForm url a = SealedForm
    { runSealedForm :: Env -> Incr (Either [String] a, Hamlet url)
    }
type SealedFormlet url a = Maybe a -> SealedForm url a
instance Functor (SealedForm url) where
    fmap f (SealedForm g) = SealedForm
                          $ \env -> liftM (first $ fmap f) (g env)
instance Applicative (SealedForm url) where
    pure a = SealedForm $ const $ return (Right a, mempty)
    (SealedForm f) <*> (SealedForm g) = SealedForm $ \env -> do
        (f1, f2) <- f env
        (g1, g2) <- g env
        return (f1 `apE` g1, f2 `mappend` g2)
      where
        apE (Left x) (Left y) = Left $ x ++ y
        apE (Left x) _ = Left x
        apE _ (Left y) = Left y
        apE (Right x) (Right y) = Right $ x y

sealForm :: ([String] -> Hamlet url -> Hamlet url)
         -> Form url a -> SealedForm url a
sealForm wrapper (Form form) = SealedForm $ \env -> liftM go (form env)
  where
    go (FormSuccess a, xml) = (Right a, wrapper [] xml)
    go (FormFailure errs, xml) = (Left errs, wrapper errs xml)
    go (FormMissing, xml) = (Left [], wrapper [] xml)

sealFormlet :: ([String] -> Hamlet url -> Hamlet url)
            -> Formlet url a -> SealedFormlet url a
sealFormlet wrapper formlet initVal = sealForm wrapper $ formlet initVal

instance Functor (Form url) where
    fmap f (Form g) = Form $ \env -> liftM (first $ fmap f) (g env)

input' :: (String -> String -> Hamlet url)
       -> Maybe String
       -> Form url String
input' mkXml val = Form $ \env -> do
    i <- incr
    let i' = show i
    let param = lookup i' env
    let xml = mkXml i' $ fromMaybe (fromMaybe "" val) param
    return (maybe FormMissing FormSuccess param, xml)

check :: Form url a -> (a -> Either [String] b) -> Form url b
check (Form form) f = Form $ \env -> liftM (first go) (form env)
  where
    go FormMissing = FormMissing
    go (FormFailure x) = FormFailure x
    go (FormSuccess a) =
        case f a of
            Left errs -> FormFailure errs
            Right b -> FormSuccess b

class Formable a where
    formable :: SealedFormlet url a

class Fieldable a where
    fieldable :: Formlet url a

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

instance Fieldable [Char] where
    fieldable = input' go
      where
        go name val = [$hamlet|
%input!type=text!name=$string.name$!value=$string.val$
|]

instance Fieldable Html where
    fieldable = fmap preEscapedString
              . input' go
              . fmap (Data.ByteString.Lazy.UTF8.toString . renderHtml)
      where
        go name val = [$hamlet|%textarea!name=$string.name$ $string.val$|]

instance Fieldable Day where
    fieldable x = input' go (fmap show x) `check` asDay
      where
        go name val = [$hamlet|
%input!type=date!name=$string.name$!value=$string.val$
|]
        asDay s = case reads s of
                    (y, _):_ -> Right y
                    [] -> Left ["Invalid day"]

newtype Slug = Slug { unSlug :: String }
    deriving (Read, Eq, Show, SinglePiece, Persistable)

instance Fieldable Slug where
    fieldable x = input' go (fmap unSlug x) `check` asSlug
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
instance Fieldable NonEmptyString where
    fieldable x = input' go (fmap unNonEmptyString x) `check` notEmpty
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
        let cols = map (upperFirst . fst) $ tableColumns t
        ap <- [|(<*>)|]
        just <- [|pure|]
        nothing <- [|Nothing|]
        let just' = just `AppE` ConE (mkName $ tableName t)
        let c1 = Clause [ConP (mkName "Nothing") []]
                        (NormalB $ go ap just' $ zip cols $ map (const nothing) cols)
                        []
        xs <- mapM (const $ newName "x") cols
        let xs' = map (AppE just . VarE) xs
        let c2 = Clause [ConP (mkName "Just") [ConP (mkName $ tableName t)
                            $ map VarP xs]]
                        (NormalB $ go ap just' $ zip cols xs')
                        []
        return $ InstanceD [] (ConT ''Formable `AppT` ConT (mkName $ tableName t))
            [FunD (mkName "formable") [c1, c2]]
    go ap just' = foldl (ap' ap) just' . map go'
    go' (label, ex) =
        VarE (mkName "sealForm") `AppE`
        (VarE (mkName "wrapperRow") `AppE` LitE (StringL label)) `AppE`
        (VarE (mkName "fieldable") `AppE` ex)
    ap' ap x y = InfixE (Just x) ap (Just y)
