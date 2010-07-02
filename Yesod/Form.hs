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
      GForm (..)
    , Form
    , FormField
    , FormResult (..)
      -- * Unwrapping functions
    , runFormGet
    , runFormPost
    , runFormGet'
    , runFormPost'
      -- * Type classes
    , IsForm (..)
    , IsFormField (..)
      -- * Field/form helpers
    , requiredField
    , mapFormXml
    , newFormIdent
      -- * Pre-built fields
    , fieldsToTable
    , stringField
    , intField
    , dayField
    , boolField
    , htmlField
    , stringInput
      -- * Template Haskell
    , share2
    , mkIsForm
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
import qualified Data.ByteString.Lazy.UTF8 as U
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
    show UrlEncoded = "application/x-www-form-urlencoded"
    show Multipart = "multipart/form-data"
instance Monoid Enctype where
    mempty = UrlEncoded
    mappend UrlEncoded UrlEncoded = UrlEncoded
    mappend _ _ = Multipart

newtype GForm sub y xml a = GForm
    { deform :: Env -> FileEnv -> StateT Int (GHandler sub y) (FormResult a, xml, Enctype)
    }
type Form sub y = GForm sub y (Widget sub y ())
type FormField sub y = GForm sub y [FieldInfo sub y]

mapFormXml :: (xml1 -> xml2) -> GForm s y xml1 a -> GForm s y xml2 a
mapFormXml f (GForm g) = GForm $ \e fe -> do
    (res, xml, enc) <- g e fe
    return (res, f xml, enc)

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

class IsForm a where
    toForm :: Maybe a -> Form sub y a
class IsFormField a where
    toFormField :: Html () -> Html () -> Maybe a -> FormField sub y a

requiredField :: FieldProfile sub y a
              -> Html () -> Html () -> Maybe a -> FormField sub y a
requiredField (FieldProfile parse render mkXml w) label tooltip orig =
  GForm $ \env _ -> do
    name <- newFormIdent
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
instance IsFormField String where
    toFormField = requiredField stringField

intField :: FieldProfile sub y Int
intField = FieldProfile
    { fpParse = maybe (Left "Invalid integer") Right . readMay
    , fpRender = show
    , fpHamlet = \name val isReq -> [$hamlet|
%input#$name$!name=$name$!type=number!:isReq:required!value=$val$
|]
    , fpWidget = \_name -> return ()
    }
instance IsFormField Int where
    toFormField = requiredField intField

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
instance IsFormField Day where
    toFormField = requiredField dayField

boolField :: Html () -> Html () -> Maybe Bool -> FormField sub y Bool
boolField label tooltip orig = GForm $ \env _ -> do
    name <- newFormIdent
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
instance IsFormField Bool where
    toFormField = boolField

htmlField :: FieldProfile sub y (Html ())
htmlField = FieldProfile
    { fpParse = Right . preEscapedString
    , fpRender = U.toString . renderHtml
    , fpHamlet = \name val isReq -> [$hamlet|
%textarea#$name$!name=$name$ $val$
|]
    , fpWidget = \name -> do
        addScriptRemote "http://js.nicedit.com/nicEdit-latest.js"
        addHead [$hamlet|%script bkLib.onDomLoaded(function(){new nicEditor({fullPanel:true}).panelInstance("$string.name$")})|]
        addStyle [$hamlet|\#$string.name${min-width:400px;min-height:300px}|]
    }
instance IsFormField (Html ()) where
    toFormField = requiredField htmlField

readMay :: Read a => String -> Maybe a
readMay s = case reads s of
                (x, _):_ -> Just x
                [] -> Nothing

--------------------- End prebuilt forms

--------------------- Begin prebuilt inputs

stringInput :: String -> Form sub master String
stringInput n = GForm $ \env _ -> return
    (case lookup n env of
        Nothing -> FormMissing
        Just "" -> FormFailure [n ++ ": You must provide a non-empty string"]
        Just x -> FormSuccess x, mempty, UrlEncoded)

--------------------- End prebuilt inputs

newFormIdent :: Monad m => StateT Int m String
newFormIdent = do
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

share2 :: Monad m => (a -> m [b]) -> (a -> m [b]) -> a -> m [b]
share2 f g a = do
    f' <- f a
    g' <- g a
    return $ f' ++ g'

mkIsForm :: [EntityDef] -> Q [Dec]
mkIsForm = mapM derive
  where
    derive :: EntityDef -> Q Dec
    derive t = do
        let fst3 (x, _, _) = x
        let cols = map (toLabel . fst3) $ entityColumns t
        ap <- [|(<*>)|]
        just <- [|pure|]
        nothing <- [|Nothing|]
        let just' = just `AppE` ConE (mkName $ entityName t)
        string' <- [|string|]
        mempty' <- [|mempty|]
        mfx <- [|mapFormXml|]
        ftt <- [|fieldsToTable|]
        let go_ = go ap just' string' mempty' mfx ftt
        let c1 = Clause [ ConP (mkName "Nothing") []
                        ]
                        (NormalB $ go_ $ zip cols $ map (const nothing) cols)
                        []
        xs <- mapM (const $ newName "x") cols
        let xs' = map (AppE just . VarE) xs
        let c2 = Clause [ ConP (mkName "Just") [ConP (mkName $ entityName t)
                            $ map VarP xs]]
                        (NormalB $ go_ $ zip cols xs')
                        []
        return $ InstanceD [] (ConT ''IsForm
                              `AppT` ConT (mkName $ entityName t))
            [FunD (mkName "toForm") [c1, c2]]
    go ap just' string' mem mfx ftt a =
        let x = foldl (ap' ap) just' $ map (go' string' mem) a
         in mfx `AppE` ftt `AppE` x
    go' string' mempty' (label, ex) =
        let label' = string' `AppE` LitE (StringL label)
         in VarE (mkName "toFormField") `AppE` label'
                `AppE` mempty' `AppE` ex
    ap' ap x y = InfixE (Just x) ap (Just y)

toLabel :: String -> String
toLabel "" = ""
toLabel (x:rest) = toUpper x : go rest
  where
    go "" = ""
    go (c:cs)
        | isUpper c = ' ' : c : go cs
        | otherwise = c : go cs
