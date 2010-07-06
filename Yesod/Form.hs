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
    , Formlet
    , FormField
    , FormletField
    , FormResult (..)
    , Enctype (..)
    , FieldInfo (..)
      -- * Newtype wrappers
    , JqueryDay (..)
    , NicHtml (..)
      -- * Unwrapping functions
    , runFormGet
    , runFormPost
    , runFormGet'
    , runFormPost'
      -- * Type classes
    , ToForm (..)
    , ToFormField (..)
      -- * Field/form helpers
    , requiredFieldHelper
    , optionalFieldHelper
    , mapFormXml
    , newFormIdent
    , fieldsToTable
      -- * Field profiles
    , FieldProfile (..)
    , stringFieldProfile
    , intFieldProfile
    , dayFieldProfile
    , timeFieldProfile
    , htmlFieldProfile
      -- * Pre-built fields
    , stringField
    , maybeStringField
    , intField
    , maybeIntField
    , doubleField
    , maybeDoubleField
    , dayField
    , maybeDayField
    , timeField
    , maybeTimeField
    , htmlField
    , maybeHtmlField
    , selectField
    , maybeSelectField
    , boolField
      -- * Pre-built inputs
    , stringInput
    , maybeStringInput
    , boolInput
    , dayInput
    , maybeDayInput
      -- * Template Haskell
    , share2
    , mkToForm
    ) where

import Text.Hamlet
import Yesod.Request
import Yesod.Handler
import Control.Applicative hiding (optional)
import Data.Time (Day, TimeOfDay (TimeOfDay))
import Data.Maybe (fromMaybe, isJust)
import "transformers" Control.Monad.IO.Class
import Control.Monad ((<=<), liftM, join)
import Data.Monoid (Monoid (..))
import Control.Monad.Trans.State
import Language.Haskell.TH.Syntax
import Database.Persist.Base (EntityDef (..), PersistField)
import Data.Char (toUpper, isUpper)
import Data.Int (Int64)
import qualified Data.ByteString.Lazy.UTF8 as U
import Yesod.Widget
import Control.Arrow ((&&&))

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
type Form sub y = GForm sub y (GWidget sub y ())
type Formlet sub y a = Maybe a -> Form sub y a
type FormField sub y = GForm sub y [FieldInfo sub y]
type FormletField sub y a = Maybe a -> FormField sub y a

mapFormXml :: (xml1 -> xml2) -> GForm s y xml1 a -> GForm s y xml2 a
mapFormXml f (GForm g) = GForm $ \e fe -> do
    (res, xml, enc) <- g e fe
    return (res, f xml, enc)

data FieldInfo sub y = FieldInfo
    { fiLabel :: Html ()
    , fiTooltip :: Html ()
    , fiIdent :: String
    , fiInput :: GWidget sub y ()
    , fiErrors :: Maybe (Html ())
    }

type Env = [(String, String)]
type FileEnv = [(String, FileInfo)]

instance Monoid xml => Functor (GForm sub url xml) where
    fmap f (GForm g) =
        GForm $ \env fe -> liftM (first3 $ fmap f) (g env fe)
      where
        first3 f' (x, y, z) = (f' x, y, z)

instance Monoid xml => Applicative (GForm sub url xml) where
    pure a = GForm $ const $ const $ return (pure a, mempty, mempty)
    (GForm f) <*> (GForm g) = GForm $ \env fe -> do
        (f1, f2, f3) <- f env fe
        (g1, g2, g3) <- g env fe
        return (f1 <*> g1, f2 `mappend` g2, f3 `mappend` g3)

fieldsToTable :: [FieldInfo sub y] -> GWidget sub y ()
fieldsToTable = mapM_ go
  where
    go fi = do
        wrapWidget (fiInput fi) $ \w -> [$hamlet|
%tr
    %td
        %label!for=$fiIdent.fi$ $fiLabel.fi$
        .tooltip $fiTooltip.fi$
    %td
        ^w^
    $maybe fiErrors.fi err
        %td.errors $err$
|]

class ToForm a where
    toForm :: Maybe a -> Form sub y a
class ToFormField a where
    toFormField :: Html () -> Html () -> Maybe a -> FormField sub y a

requiredFieldHelper :: FieldProfile sub y a
              -> Html () -> Html () -> Maybe a -> FormField sub y a
requiredFieldHelper (FieldProfile parse render mkXml w) label tooltip orig =
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
                            FormFailure [x] -> Just $ string x
                            _ -> Nothing
            }
    return (res, [fi], UrlEncoded)

optionalFieldHelper :: FieldProfile sub y a
              -> Html () -> Html () -> Maybe (Maybe a)
              -> FormField sub y (Maybe a)
optionalFieldHelper (FieldProfile parse render mkXml w) label tooltip orig' =
  GForm $ \env _ -> do
    let orig = join orig'
    name <- newFormIdent
    let (res, val) =
            if null env
                then (FormMissing, maybe "" render orig)
                else case lookup name env of
                        Nothing -> (FormSuccess Nothing, "")
                        Just "" -> (FormSuccess Nothing, "")
                        Just x ->
                            case parse x of
                                Left e -> (FormFailure [e], x)
                                Right y -> (FormSuccess $ Just y, x)
    let fi = FieldInfo
            { fiLabel = label
            , fiTooltip = tooltip
            , fiIdent = name
            , fiInput = w name >> addBody (mkXml (string name) (string val) False)
            , fiErrors = case res of
                            FormFailure [x] -> Just $ string x
                            _ -> Nothing
            }
    return (res, [fi], UrlEncoded)

data FieldProfile sub y a = FieldProfile
    { fpParse :: String -> Either String a
    , fpRender :: a -> String
    , fpHamlet :: Html () -> Html () -> Bool -> Hamlet (Routes y)
    , fpWidget :: String -> GWidget sub y ()
    }

--------------------- Begin prebuilt forms

stringField :: Html () -> Html () -> FormletField sub y String
stringField = requiredFieldHelper stringFieldProfile

maybeStringField :: Html () -> Html () -> FormletField sub y (Maybe String)
maybeStringField = optionalFieldHelper stringFieldProfile

stringFieldProfile :: FieldProfile sub y String
stringFieldProfile = FieldProfile
    { fpParse = Right
    , fpRender = id
    , fpHamlet = \name val isReq -> [$hamlet|
%input#$name$!name=$name$!type=text!:isReq:required!value=$val$
|]
    , fpWidget = \_name -> return ()
    }
instance ToFormField String where
    toFormField = requiredFieldHelper stringFieldProfile
instance ToFormField (Maybe String) where
    toFormField = optionalFieldHelper stringFieldProfile

intField :: Html () -> Html () -> FormletField sub y Int
intField = requiredFieldHelper intFieldProfile

maybeIntField :: Html () -> Html () -> FormletField sub y (Maybe Int)
maybeIntField = optionalFieldHelper intFieldProfile

intFieldProfile :: Integral i => FieldProfile sub y i
intFieldProfile = FieldProfile
    { fpParse = maybe (Left "Invalid integer") Right . readMayI
    , fpRender = showI
    , fpHamlet = \name val isReq -> [$hamlet|
%input#$name$!name=$name$!type=number!:isReq:required!value=$val$
|]
    , fpWidget = \_name -> return ()
    }
  where
    showI x = show (fromIntegral x :: Integer)
    readMayI s = case reads s of
                    (x, _):_ -> Just $ fromInteger x
                    [] -> Nothing
instance ToFormField Int where
    toFormField = requiredFieldHelper intFieldProfile
instance ToFormField (Maybe Int) where
    toFormField = optionalFieldHelper intFieldProfile
instance ToFormField Int64 where
    toFormField = requiredFieldHelper intFieldProfile
instance ToFormField (Maybe Int64) where
    toFormField = optionalFieldHelper intFieldProfile

doubleField :: Html () -> Html () -> FormletField sub y Double
doubleField = requiredFieldHelper doubleFieldProfile

maybeDoubleField :: Html () -> Html () -> FormletField sub y (Maybe Double)
maybeDoubleField = optionalFieldHelper doubleFieldProfile

doubleFieldProfile :: FieldProfile sub y Double
doubleFieldProfile = FieldProfile
    { fpParse = maybe (Left "Invalid number") Right . readMay
    , fpRender = show
    , fpHamlet = \name val isReq -> [$hamlet|
%input#$name$!name=$name$!type=number!:isReq:required!value=$val$
|]
    , fpWidget = \_name -> return ()
    }
instance ToFormField Double where
    toFormField = requiredFieldHelper doubleFieldProfile
instance ToFormField (Maybe Double) where
    toFormField = optionalFieldHelper doubleFieldProfile

dayField :: Html () -> Html () -> FormletField sub y Day
dayField = requiredFieldHelper dayFieldProfile

maybeDayField :: Html () -> Html () -> FormletField sub y (Maybe Day)
maybeDayField = optionalFieldHelper dayFieldProfile

dayFieldProfile :: FieldProfile sub y Day
dayFieldProfile = FieldProfile
    { fpParse = maybe (Left "Invalid day, must be in YYYY-MM-DD format") Right
              . readMay
    , fpRender = show
    , fpHamlet = \name val isReq -> [$hamlet|
%input#$name$!name=$name$!type=date!:isReq:required!value=$val$
|]
    , fpWidget = const $ return ()
    }
instance ToFormField Day where
    toFormField = requiredFieldHelper dayFieldProfile
instance ToFormField (Maybe Day) where
    toFormField = optionalFieldHelper dayFieldProfile

jqueryDayFieldProfile :: FieldProfile sub y JqueryDay
jqueryDayFieldProfile = FieldProfile
    { fpParse = maybe
                  (Left "Invalid day, must be in YYYY-MM-DD format")
                  (Right . JqueryDay)
              . readMay
    , fpRender = show . unJqueryDay
    , fpHamlet = \name val isReq -> [$hamlet|
%input#$name$!name=$name$!type=date!:isReq:required!value=$val$
|]
    , fpWidget = \name -> do
        addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js"
        addScriptRemote "http://ajax.googleapis.com/ajax/libs/jqueryui/1.8.1/jquery-ui.min.js"
        addStylesheetRemote "http://ajax.googleapis.com/ajax/libs/jqueryui/1.8.1/themes/cupertino/jquery-ui.css"
        addHead [$hamlet|%script $$(function(){$$("#$name$").datepicker({dateFormat:'yy-mm-dd'})})|]
    }

-- | A newtype wrapper around 'Day', using jQuery UI date picker for the
-- 'ToFormField' instance.
newtype JqueryDay = JqueryDay { unJqueryDay :: Day }
    deriving PersistField
instance ToFormField JqueryDay where
    toFormField = requiredFieldHelper jqueryDayFieldProfile
instance ToFormField (Maybe JqueryDay) where
    toFormField = optionalFieldHelper jqueryDayFieldProfile

parseTime :: String -> Either String TimeOfDay
parseTime (h2:':':m1:m2:[]) = parseTimeHelper ('0', h2, m1, m2, '0', '0')
parseTime (h1:h2:':':m1:m2:[]) = parseTimeHelper (h1, h2, m1, m2, '0', '0')
parseTime (h1:h2:':':m1:m2:':':s1:s2:[]) =
    parseTimeHelper (h1, h2, m1, m2, s1, s2)
parseTime _ = Left "Invalid time, must be in HH:MM[:SS] format"

parseTimeHelper :: (Char, Char, Char, Char, Char, Char)
                -> Either [Char] TimeOfDay
parseTimeHelper (h1, h2, m1, m2, s1, s2)
    | h < 0 || h > 23 = Left $ "Invalid hour: " ++ show h
    | m < 0 || m > 59 = Left $ "Invalid minute: " ++ show m
    | s < 0 || s > 59 = Left $ "Invalid second: " ++ show s
    | otherwise = Right $ TimeOfDay h m s
  where
    h = read [h1, h2]
    m = read [m1, m2]
    s = fromInteger $ read [s1, s2]

timeField :: Html () -> Html () -> FormletField sub y TimeOfDay
timeField = requiredFieldHelper timeFieldProfile

maybeTimeField :: Html () -> Html () -> FormletField sub y (Maybe TimeOfDay)
maybeTimeField = optionalFieldHelper timeFieldProfile

timeFieldProfile :: FieldProfile sub y TimeOfDay
timeFieldProfile = FieldProfile
    { fpParse = parseTime
    , fpRender = show
    , fpHamlet = \name val isReq -> [$hamlet|
%input#$name$!name=$name$!:isReq:required!value=$val$
|]
    , fpWidget = const $ return ()
    }
instance ToFormField TimeOfDay where
    toFormField = requiredFieldHelper timeFieldProfile
instance ToFormField (Maybe TimeOfDay) where
    toFormField = optionalFieldHelper timeFieldProfile

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
%input#$name$!type=checkbox!name=$name$!:val:checked
|]
            , fiErrors = case res of
                            FormFailure [x] -> Just $ string x
                            _ -> Nothing
            }
    return (res, [fi], UrlEncoded)
instance ToFormField Bool where
    toFormField = boolField

htmlField :: Html () -> Html () -> FormletField sub y (Html ())
htmlField = requiredFieldHelper htmlFieldProfile

maybeHtmlField :: Html () -> Html () -> FormletField sub y (Maybe (Html ()))
maybeHtmlField = optionalFieldHelper htmlFieldProfile

htmlFieldProfile :: FieldProfile sub y (Html ())
htmlFieldProfile = FieldProfile
    { fpParse = Right . preEscapedString
    , fpRender = U.toString . renderHtml
    , fpHamlet = \name val _isReq -> [$hamlet|
%textarea.html#$name$!name=$name$ $val$
|]
    , fpWidget = const $ return ()
    }
instance ToFormField (Html ()) where
    toFormField = requiredFieldHelper htmlFieldProfile
instance ToFormField (Maybe (Html ())) where
    toFormField = optionalFieldHelper htmlFieldProfile

newtype NicHtml = NicHtml { unNicHtml :: Html () }
    deriving PersistField

nicHtmlFieldProfile :: FieldProfile sub y NicHtml
nicHtmlFieldProfile = FieldProfile
    { fpParse = Right . NicHtml . preEscapedString
    , fpRender = U.toString . renderHtml . unNicHtml
    , fpHamlet = \name val _isReq -> [$hamlet|
%textarea.html#$name$!name=$name$ $val$
|]
    , fpWidget = \name -> do
        addScriptRemote "http://js.nicedit.com/nicEdit-latest.js"
        addHead [$hamlet|%script bkLib.onDomLoaded(function(){new nicEditor({fullPanel:true}).panelInstance("$name$")})|]
    }
instance ToFormField NicHtml where
    toFormField = requiredFieldHelper nicHtmlFieldProfile
instance ToFormField (Maybe NicHtml) where
    toFormField = optionalFieldHelper nicHtmlFieldProfile

readMay :: Read a => String -> Maybe a
readMay s = case reads s of
                (x, _):_ -> Just x
                [] -> Nothing

selectField :: Eq x => [(x, String)]
            -> Html () -> Html ()
            -> Maybe x -> FormField sub master x
selectField pairs label tooltip initial = GForm $ \env _ -> do
    i <- newFormIdent
    let pairs' = zip [1 :: Int ..] pairs
    let res = case lookup i env of
                Nothing -> FormMissing
                Just "none" -> FormFailure ["Field is required"]
                Just x ->
                    case reads x of
                        (x', _):_ ->
                            case lookup x' pairs' of
                                Nothing -> FormFailure ["Invalid entry"]
                                Just (y, _) -> FormSuccess y
                        [] -> FormFailure ["Invalid entry"]
    let isSelected x =
            case res of
                FormSuccess y -> x == y
                _ -> Just x == initial
    let input = [$hamlet|
%select#$i$!name=$i$
    %option!value=none
    $forall pairs' pair
        %option!value=$show.fst.pair$!:isSelected.fst.snd.pair:selected $snd.snd.pair$
|]
    let fi = FieldInfo
            { fiLabel = label
            , fiTooltip = tooltip
            , fiIdent = i
            , fiInput = addBody input
            , fiErrors = case res of
                            FormFailure [x] -> Just $ string x
                            _ -> Nothing
            }
    return (res, [fi], UrlEncoded)

maybeSelectField :: Eq x => [(x, String)]
                 -> Html () -> Html ()
                 -> Maybe x -> FormField sub master (Maybe x)
maybeSelectField pairs label tooltip initial = GForm $ \env _ -> do
    i <- newFormIdent
    let pairs' = zip [1 :: Int ..] pairs
    let res = case lookup i env of
                Nothing -> FormMissing
                Just "none" -> FormSuccess Nothing
                Just x ->
                    case reads x of
                        (x', _):_ ->
                            case lookup x' pairs' of
                                Nothing -> FormFailure ["Invalid entry"]
                                Just (y, _) -> FormSuccess $ Just y
                        [] -> FormFailure ["Invalid entry"]
    let isSelected x =
            case res of
                FormSuccess y -> Just x == y
                _ -> Just x == initial
    let input = [$hamlet|
%select#$i$!name=$i$
    %option!value=none
    $forall pairs' pair
        %option!value=$show.fst.pair$!:isSelected.fst.snd.pair:selected $snd.snd.pair$
|]
    let fi = FieldInfo
            { fiLabel = label
            , fiTooltip = tooltip
            , fiIdent = i
            , fiInput = addBody input
            , fiErrors = case res of
                            FormFailure [x] -> Just $ string x
                            _ -> Nothing
            }
    return (res, [fi], UrlEncoded)

--------------------- End prebuilt forms

--------------------- Begin prebuilt inputs

stringInput :: String -> Form sub master String
stringInput n = GForm $ \env _ -> return
    (case lookup n env of
        Nothing -> FormMissing
        Just "" -> FormFailure [n ++ ": You must provide a non-empty string"]
        Just x -> FormSuccess x, mempty, UrlEncoded)

maybeStringInput :: String -> Form sub master (Maybe String)
maybeStringInput n = GForm $ \env _ -> return
    (case lookup n env of
        Nothing -> FormSuccess Nothing
        Just "" -> FormSuccess Nothing
        Just x -> FormSuccess $ Just x, mempty, UrlEncoded)

boolInput :: String -> Form sub master Bool
boolInput n = GForm $ \env _ -> return
    (FormSuccess $ isJust $ lookup n env, mempty, UrlEncoded)

dayInput :: String -> Form sub master Day
dayInput n = GForm $ \env _ -> return
    (case lookup n env of
        Nothing -> FormMissing
        Just "" -> FormFailure [n ++ ": You must provide a non-empty string"]
        Just x ->
            case readMay x of
                Just y -> FormSuccess y
                Nothing -> FormFailure [n ++ ": Invalid date"]
        , mempty, UrlEncoded)

maybeDayInput :: String -> Form sub master (Maybe Day)
maybeDayInput n = GForm $ \env _ -> return
    (case lookup n env of
        Nothing -> FormSuccess Nothing
        Just "" -> FormSuccess Nothing
        Just x ->
            case readMay x of
                Just y -> FormSuccess $ Just y
                Nothing -> FormFailure [n ++ ": Invalid date"]
        , mempty, UrlEncoded)

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

mkToForm :: [EntityDef] -> Q [Dec]
mkToForm = mapM derive
  where
    getLabel (x, _, z) = fromMaybe (toLabel x) $ getLabel' z
    getLabel' [] = Nothing
    getLabel' (('l':'a':'b':'e':'l':'=':x):_) = Just x
    getLabel' (_:x) = getLabel' x
    getTooltip (_, _, z) = fromMaybe "" $ getTooltip' z
    getTooltip' (('t':'o':'o':'l':'t':'i':'p':'=':x):_) = Just x
    getTooltip' (_:x) = getTooltip' x
    getTooltip' [] = Nothing
    derive :: EntityDef -> Q Dec
    derive t = do
        let cols = map (getLabel &&& getTooltip) $ entityColumns t
        ap <- [|(<*>)|]
        just <- [|pure|]
        nothing <- [|Nothing|]
        let just' = just `AppE` ConE (mkName $ entityName t)
        string' <- [|string|]
        mfx <- [|mapFormXml|]
        ftt <- [|fieldsToTable|]
        let go_ = go ap just' string' mfx ftt
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
        return $ InstanceD [] (ConT ''ToForm
                              `AppT` ConT (mkName $ entityName t))
            [FunD (mkName "toForm") [c1, c2]]
    go ap just' string' mfx ftt a =
        let x = foldl (ap' ap) just' $ map (go' string') a
         in mfx `AppE` ftt `AppE` x
    go' string' ((label, tooltip), ex) =
        let label' = string' `AppE` LitE (StringL label)
            tooltip' = string' `AppE` LitE (StringL tooltip)
         in VarE (mkName "toFormField") `AppE` label'
                `AppE` tooltip' `AppE` ex
    ap' ap x y = InfixE (Just x) ap (Just y)

toLabel :: String -> String
toLabel "" = ""
toLabel (x:rest) = toUpper x : go rest
  where
    go "" = ""
    go (c:cs)
        | isUpper c = ' ' : c : go cs
        | otherwise = c : go cs
