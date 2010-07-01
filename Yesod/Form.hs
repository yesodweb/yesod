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
    , FormResult (..)
      -- * Unwrapping functions
    , runFormGet
    , runFormPost
    , runFormGet'
    , runFormPost'
    , requiredField
    , stringField
    , intField
    , dayField
    , boolField
    , fieldsToTable
    {- FIXME
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
      -- * Pre-built form
    , optionalField
    , requiredField
    , notEmptyField
    , boolField
    -}
    ) where

import Text.Hamlet
import Yesod.Request
import Yesod.Handler
import Control.Applicative hiding (optional)
import Data.Time (Day)
import Data.Maybe (fromMaybe)
import "transformers" Control.Monad.IO.Class
import Control.Monad ((<=<), liftM, join)
import Data.Monoid (Monoid (..))
import Control.Monad.Trans.State
import Control.Arrow (first)
import Language.Haskell.TH.Syntax
import Database.Persist.Base (PersistField, EntityDef (..))
import Data.Char (isAlphaNum, toUpper, isUpper)
import Data.Maybe (isJust)
import Web.Routes.Quasi (SinglePiece)
import Data.Int (Int64)
import qualified Data.ByteString.Lazy.UTF8
import Yesod.Widget

data FormResult a = FormMissing
                  | FormFailure [String]
                  | FormSuccess a
    deriving Show
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

data Enctype = UrlEncoded | Multipart
instance Show Enctype where
    show UrlEncoded = "urlencoded"
    show Multipart = "multipart/mimetype" -- FIXME
instance Monoid Enctype where
    mempty = UrlEncoded
    mappend UrlEncoded UrlEncoded = UrlEncoded
    mappend _ _ = Multipart

newtype GForm sub y xml a = GForm
    { deform :: Env -> FileEnv -> StateT Int (GHandler sub y) (FormResult a, xml, Enctype)
    }
type Form sub y = GForm sub y (Widget sub y ())
type FormField sub y = GForm sub y [FieldInfo sub y]

data FieldInfo sub y = FieldInfo
    { fiLabel :: Html ()
    , fiTooltip :: Html ()
    , fiIdent :: String
    , fiInput :: Widget sub y ()
    , fiErrors :: Html ()
    }

type Env = [(String, String)]
type FileEnv = [(String, FileInfo)]

instance Monoid xml => Functor (GForm sub url xml) where
    fmap f (GForm g) =
        GForm $ \env fe -> liftM (first3 $ fmap f) (g env fe)
      where
        first3 f (x, y, z) = (f x, y, z)

instance Monoid xml => Applicative (GForm sub url xml) where
    pure a = GForm $ const $ const $ return (pure a, mempty, mempty)
    (GForm f) <*> (GForm g) = GForm $ \env fe -> do
        (f1, f2, f3) <- f env fe
        (g1, g2, g3) <- g env fe
        return (f1 <*> g1, f2 `mappend` g2, f3 `mappend` g3)

fieldsToTable :: [FieldInfo sub y] -> Widget sub y ()
fieldsToTable = mapM_ go
  where
    go fi = do
        flip wrapWidget (fiInput fi) $ \w -> [$hamlet|
%tr
    %td
        %label!for=$string.fiIdent.fi$ $fiLabel.fi$
        .tooltip $fiTooltip.fi$
    %td
        ^w^
    %td.errors
        $fiErrors.fi$
|]

requiredField :: FieldProfile sub y a
              -> Html () -> Html () -> Maybe a -> FormField sub y a
requiredField (FieldProfile parse render mkXml w) label tooltip orig =
  GForm $ \env _ -> do
    name <- incr
    let (res, val) =
            if null env
                then (FormMissing, maybe "" render orig)
                else case lookup name env of
                        Nothing -> (FormMissing, "")
                        Just "" -> (FormFailure ["Value is required"], "")
                        Just x ->
                            case parse x of
                                Left e -> (FormFailure [e], x)
                                Right y -> (FormSuccess y, x)
    let fi = FieldInfo
            { fiLabel = label
            , fiTooltip = tooltip
            , fiIdent = name
            , fiInput = w name >> addBody (mkXml (string name) (string val) True)
            , fiErrors = case res of
                            FormFailure [x] -> string x
                            _ -> string ""
            }
    return (res, [fi], UrlEncoded)

data FieldProfile sub y a = FieldProfile
    { fpParse :: String -> Either String a
    , fpRender :: a -> String
    , fpHamlet :: Html () -> Html () -> Bool -> Hamlet (Routes y)
    , fpWidget :: String -> Widget sub y ()
    }

--------------------- Begin prebuilt forms

stringField :: FieldProfile sub y String
stringField = FieldProfile
    { fpParse = Right
    , fpRender = id
    , fpHamlet = \name val isReq -> [$hamlet|
%input#$name$!name=$name$!type=text!:isReq:required!value=$val$
|]
    , fpWidget = \_name -> return ()
    }

intField :: FieldProfile sub y Int
intField = FieldProfile
    { fpParse = maybe (Left "Invalid integer") Right . readMay
    , fpRender = show
    , fpHamlet = \name val isReq -> [$hamlet|
%input#$name$!name=$name$!type=number!:isReq:required!value=$val$
|]
    , fpWidget = \_name -> return ()
    }

dayField :: FieldProfile sub y Day
dayField = FieldProfile
    { fpParse = maybe (Left "Invalid day, must be in YYYY-MM-DD format") Right
              . readMay
    , fpRender = show
    , fpHamlet = \name val isReq -> [$hamlet|
%input#$name$!name=$name$!type=date!:isReq:required!value=$val$
|]
    , fpWidget = \name -> do
        addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js"
        addScriptRemote "http://ajax.googleapis.com/ajax/libs/jqueryui/1.8.1/jquery-ui.min.js"
        addStylesheetRemote "http://ajax.googleapis.com/ajax/libs/jqueryui/1.8.1/themes/cupertino/jquery-ui.css"
        addHead [$hamlet|%script $$(function(){$$("#$string.name$").datepicker({dateFormat:'yy-mm-dd'})})|]
    }

boolField :: Html () -> Html () -> Maybe Bool -> FormField sub y Bool
boolField label tooltip orig = GForm $ \env _ -> do
    name <- incr
    let (res, val) =
            if null env
                then (FormMissing, fromMaybe False orig)
                else case lookup name env of
                        Nothing -> (FormSuccess False, False)
                        Just _ -> (FormSuccess True, True)
    let fi = FieldInfo
            { fiLabel = label
            , fiTooltip = tooltip
            , fiIdent = name
            , fiInput = addBody [$hamlet|
%input#$string.name$!type=checkbox!name=$string.name$!:val:checked
|]
            , fiErrors = case res of
                            FormFailure [x] -> string x
                            _ -> string ""
            }
    return (res, [fi], UrlEncoded)

readMay :: Read a => String -> Maybe a
readMay s = case reads s of
                (x, _):_ -> Just x
                [] -> Nothing

--------------------- End prebuilt forms

incr :: Monad m => StateT Int m String
incr = do
    i <- get
    let i' = i + 1
    put i'
    return $ "f" ++ show i'

runFormGeneric :: Env
               -> FileEnv
               -> GForm sub y xml a
               -> GHandler sub y (FormResult a, xml, Enctype)
runFormGeneric env fe f = evalStateT (deform f env fe) 1

-- | Run a form against POST parameters.
runFormPost :: GForm sub y xml a
            -> GHandler sub y (FormResult a, xml, Enctype)
runFormPost f = do
    rr <- getRequest
    (pp, files) <- liftIO $ reqRequestBody rr
    runFormGeneric pp files f

-- | Run a form against POST parameters, disregarding the resulting HTML and
-- returning an error response on invalid input.
runFormPost' :: GForm sub y xml a -> GHandler sub y a
runFormPost' = helper <=< runFormPost

-- | Run a form against GET parameters, disregarding the resulting HTML and
-- returning an error response on invalid input.
runFormGet' :: Form sub y a -> GHandler sub y a
runFormGet' = helper <=< runFormGet

helper :: (FormResult a, b, c) -> GHandler sub y a
helper (FormSuccess a, _, _) = return a
helper (FormFailure e, _, _) = invalidArgs e
helper (FormMissing, _, _) = invalidArgs ["No input found"]

-- | Run a form against GET parameters.
runFormGet :: GForm sub y xml a
           -> GHandler sub y (FormResult a, xml, Enctype)
runFormGet f = do
    gs <- reqGetParams `fmap` getRequest
    runFormGeneric gs [] f

{-

-------- Prebuilt
optionalField :: String -> Form sub master (Maybe String)
optionalField n = Form $ \env _ ->
    return (FormSuccess $ lookup n env, mempty) -- FIXME

requiredField :: String -> Form sub master String
requiredField n = Form $ \env _ ->
    return (maybe FormMissing FormSuccess $ lookup n env, mempty) -- FIXME

notEmptyField :: String -> Form sub master String
notEmptyField n = Form $ \env _ -> return
    (case lookup n env of
        Nothing -> FormMissing
        Just "" -> FormFailure [n ++ ": You must provide a non-empty string"]
        Just x -> FormSuccess x, mempty) -- FIXME

boolField :: String -> Form sub master Bool
boolField n = Form $ \env _ -> return
    (FormSuccess $ isJust $ lookup n env, mempty) -- FIXME

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
    formable x = Form $ \env _ -> do
        i <- incr
        let param = lookup i env
        let def = if null env then fromMaybe False x else isJust param
        return (FormSuccess $ isJust param, go i def)
      where
        go name val = addBody [$hamlet|
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
-}
