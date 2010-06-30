{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Parse forms (and query strings).
module Yesod.Form
    ( -- * Data types
      Form (..)
    , Formlet
    , FormResult (..)
      -- * Unwrapping functions
    , runFormGet
    , runFormPost
    , runFormGet'
    , runFormPost'
      -- * Create your own formlets
    , incr
    , input
    , check
      -- * Error display
    , wrapperRow
    , sealFormlet
    , sealForm
    , sealRow
      -- * Formable
    , Formable (..)
    , deriveFormable
    , share2
      -- * Pre-built formlets
    ) where

import Text.Hamlet
import Yesod.Request
import Yesod.Handler
import Control.Applicative hiding (optional)
import Data.Time (Day)
import Data.Maybe (fromMaybe)
import "transformers" Control.Monad.IO.Class
import Yesod.Internal
import Control.Monad.Attempt
import Control.Monad ((<=<), liftM, join)
import Data.Monoid (mempty, mappend)
import Control.Monad.Trans.State
import Control.Arrow (first)
import Language.Haskell.TH.Syntax
import Database.Persist.Base (PersistField, EntityDef (..))
import Data.Char (isAlphaNum, toUpper, isUpper)
import Data.Maybe (fromMaybe, isJust)
import Web.Routes.Quasi (SinglePiece)
import Data.Int (Int64)
import qualified Data.ByteString.Lazy.UTF8

noParamNameError :: String
noParamNameError = "No param name (miscalling of Yesod.Form library)"

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

type Env = [(String, String)]

instance Functor (Form sub url) where
    fmap f (Form g) = Form $ \env -> liftM (first $ fmap f) (g env)

instance Applicative (Form sub url) where
    pure a = Form $ const $ return (pure a, mempty)
    (Form f) <*> (Form g) = Form $ \env -> do
        (f1, f2) <- f env
        (g1, g2) <- g env
        return (f1 <*> g1, f2 `mappend` g2)

runFormGeneric :: Env
               -> Form sub y a
               -> GHandler sub y (FormResult a, Hamlet (Routes y))
runFormGeneric env f = evalStateT (deform f env) 1

-- | Run a form against POST parameters.
runFormPost :: Form sub y a
            -> GHandler sub y (FormResult a, Hamlet (Routes y))
runFormPost f = do
    rr <- getRequest
    (pp, _) <- liftIO $ reqRequestBody rr
    runFormGeneric pp f

-- | Run a form against POST parameters, disregarding the resulting HTML and
-- returning an error response on invalid input.
runFormPost' :: Form sub y a -> GHandler sub y a
runFormPost' = helper <=< runFormPost

-- | Run a form against GET parameters, disregarding the resulting HTML and
-- returning an error response on invalid input.
runFormGet' :: Form sub y a -> GHandler sub y a
runFormGet' = helper <=< runFormGet

helper :: (FormResult a, Hamlet (Routes y)) -> GHandler sub y a
helper (FormSuccess a, _) = return a
helper (FormFailure e, _) = invalidArgs e
helper (FormMissing, _) = invalidArgs ["No input found"]

-- | Run a form against GET parameters.
runFormGet :: Form sub y a
           -> GHandler sub y (FormResult a, Hamlet (Routes y))
runFormGet f = do
    gets <- reqGetParams `fmap` getRequest
    runFormGeneric gets f

type Incr = StateT Int

incr :: Monad m => Incr m Int
incr = do
    i <- get
    let i' = i + 1
    put i'
    return i'

input :: (String -> String -> Hamlet (Routes y))
      -> Maybe String
      -> Form sub y String
input mkXml val = Form $ \env -> do
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

sealRow :: Formable b => String -> (a -> b) -> Maybe a -> Form sub master b
sealRow label getVal val =
    sealForm (wrapperRow label) $ formable $ fmap getVal val

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

class Formable a where
    formable :: Formlet sub master a

--------------- Formable instances
instance Formable String where
    formable x = input go x `check` notEmpty
      where
        go name val = [$hamlet|
%input!type=text!name=$string.name$!value=$string.val$
|]
        notEmpty s
            | null s = Left ["Value required"]
            | otherwise = Right s

instance Formable (Maybe String) where
    formable x = input go (join x) `check` isEmpty
      where
        go name val = [$hamlet|
%input!type=text!name=$string.name$!value=$string.val$
|]
        isEmpty s
            | null s = Right Nothing
            | otherwise = Right $ Just s

instance Formable (Html ()) where
    formable = fmap preEscapedString
              . input go
              . fmap (Data.ByteString.Lazy.UTF8.toString . renderHtml)
      where
        go name val = [$hamlet|%textarea!name=$string.name$ $string.val$|]

instance Formable Day where
    formable x = input go (fmap show x) `check` asDay
      where
        go name val = [$hamlet|
%input!type=date!name=$string.name$!value=$string.val$
|]
        asDay s = case reads s of
                    (y, _):_ -> Right y
                    [] -> Left ["Invalid day"]

instance Formable Int64 where
    formable x = input go (fmap show x) `check` asInt
      where
        go name val = [$hamlet|
%input!type=number!name=$string.name$!value=$string.val$
|]
        asInt s = case reads s of
                    (y, _):_ -> Right y
                    [] -> Left ["Invalid integer"]

instance Formable Double where
    formable x = input go (fmap numstring x) `check` asDouble
      where
        go name val = [$hamlet|
%input!type=number!name=$string.name$!value=$string.val$
|]
        asDouble s = case reads s of
                    (y, _):_ -> Right y
                    [] -> Left ["Invalid double"]
        numstring d =
            let s = show d
             in case reverse s of
                    '0':'.':y -> reverse y
                    _ -> s

instance Formable (Maybe Day) where
    formable x = input go (fmap show $ join x) `check` asDay
      where
        go name val = [$hamlet|
%input!type=date!name=$string.name$!value=$string.val$
|]
        asDay "" = Right Nothing
        asDay s = case reads s of
                    (y, _):_ -> Right $ Just y
                    [] -> Left ["Invalid day"]

instance Formable (Maybe Int) where
    formable x = input go (fmap show $ join x) `check` asInt
      where
        go name val = [$hamlet|
%input!type=number!name=$string.name$!value=$string.val$
|]
        asInt "" = Right Nothing
        asInt s = case reads s of
                    (y, _):_ -> Right $ Just y
                    [] -> Left ["Invalid integer"]

instance Formable (Maybe Int64) where
    formable x = input go (fmap show $ join x) `check` asInt
      where
        go name val = [$hamlet|
%input!type=number!name=$string.name$!value=$string.val$
|]
        asInt "" = Right Nothing
        asInt s = case reads s of
                    (y, _):_ -> Right $ Just y
                    [] -> Left ["Invalid integer"]

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
    formable x = input go (fmap show x) `check` asInt
      where
        go name val = [$hamlet|
%input!type=number!name=$string.name$!value=$string.val$
|]
        asInt s = case reads s of
                    (y, _):_ -> Right y
                    [] -> Left ["Invalid integer"]

newtype Slug = Slug { unSlug :: String }
    deriving (Read, Eq, Show, SinglePiece, PersistField)

instance Formable Slug where
    formable x = input go (fmap unSlug x) `check` asSlug
      where
        go name val = [$hamlet|
%input!type=text!name=$string.name$!value=$string.val$
|]
        asSlug [] = Left ["Slug must be non-empty"]
        asSlug x'
            | all (\c -> c `elem` "-_" || isAlphaNum c) x' =
                Right $ Slug x'
            | otherwise = Left ["Slug must be alphanumeric, - and _"]

share2 :: Monad m => (a -> m [b]) -> (a -> m [b]) -> a -> m [b]
share2 f g a = do
    f' <- f a
    g' <- g a
    return $ f' ++ g'

deriveFormable :: [EntityDef] -> Q [Dec]
deriveFormable = mapM derive
  where
    derive :: EntityDef -> Q Dec
    derive t = do
        let fst3 (x, _, _) = x
        let cols = map (toLabel . fst3) $ entityColumns t
        ap <- [|(<*>)|]
        just <- [|pure|]
        nothing <- [|Nothing|]
        let just' = just `AppE` ConE (mkName $ entityName t)
        let c1 = Clause [ ConP (mkName "Nothing") []
                        ]
                        (NormalB $ go ap just' $ zip cols $ map (const nothing) cols)
                        []
        xs <- mapM (const $ newName "x") cols
        let xs' = map (AppE just . VarE) xs
        let c2 = Clause [ ConP (mkName "Just") [ConP (mkName $ entityName t)
                            $ map VarP xs]]
                        (NormalB $ go ap just' $ zip cols xs')
                        []
        return $ InstanceD [] (ConT ''Formable
                              `AppT` ConT (mkName $ entityName t))
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
