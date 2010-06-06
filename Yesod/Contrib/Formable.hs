{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Yesod.Contrib.Formable where

import Text.Formlets
import Text.Hamlet
import Text.Hamlet.Monad (htmlContentToByteString)
import Data.Time (Day)
import Control.Applicative
import Control.Applicative.Error
import Web.Routes.Quasi (SinglePiece)
import Database.Persist (Persistable)
import Data.Char (isAlphaNum)
import Language.Haskell.TH.Syntax
import Database.Persist (Table (..))
import Database.Persist.Helper (upperFirst)
import Data.Convertible.Text (cs)

class Formable a where
    formable :: (Functor m, Applicative m, Monad m)
             => Formlet (Hamlet url) m a

class Fieldable a where
    fieldable :: (Functor m, Applicative m, Monad m)
              => String -> Formlet (Hamlet url) m a

pack' :: String -> HtmlContent
pack' = Unencoded . cs

instance Fieldable [Char] where
    fieldable label = input' go
      where
        go name val = [$hamlet|
%tr
    %th $pack'.label$
    %td
        %input!type=text!name=$pack'.name$!value=$pack'.val$
|]

instance Fieldable HtmlContent where
    fieldable label =
        fmap (Encoded . cs)
      . input' go
      . fmap (cs . htmlContentToByteString)
      where
        go name val = [$hamlet|
%tr
    %th $pack'.label$
    %td
        %textarea!name=$pack'.name$
            $pack'.val$
|]

instance Fieldable Day where
    fieldable label x = input' go (fmap show x) `check` asDay
      where
        go name val = [$hamlet|
%tr
    %th $pack'.label$
    %td
        %input!type=date!name=$pack'.name$!value=$pack'.val$
|]
        asDay s = maybeRead' s "Invalid day"

newtype Slug = Slug { unSlug :: String }
    deriving (Read, Eq, Show, SinglePiece, Persistable)

instance Fieldable Slug where
    fieldable label x = input' go (fmap unSlug x) `check` asSlug
      where
        go name val = [$hamlet|
%tr
    %th $pack'.label$
    %td
        %input!type=text!name=$pack'.name$!value=$pack'.val$
|]
        asSlug [] = Failure ["Slug must be non-empty"]
        asSlug x'
            | all (\c -> c `elem` "-_" || isAlphaNum c) x' =
                Success $ Slug x'
            | otherwise = Failure ["Slug must be alphanumeric, - and _"]

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
    go' (label, ex) = VarE (mkName "fieldable") `AppE` LitE (StringL label) `AppE` ex
    ap' ap x y = InfixE (Just x) ap (Just y)
