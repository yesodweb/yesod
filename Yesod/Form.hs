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
    , FormInput
    , FormResult (..)
    , Enctype (..)
    , FieldInfo (..)
      -- * Newtype wrappers
    , JqueryDay (..)
    , NicHtml (..)
    , Html'
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
    , fieldsToPlain
    , fieldsToInput
      -- * Field profiles
    , FieldProfile (..)
    , stringFieldProfile
    , intFieldProfile
    , dayFieldProfile
    , jqueryDayFieldProfile
    , timeFieldProfile
    , htmlFieldProfile
    , emailFieldProfile
      -- * Pre-built fields
    , stringField
    , maybeStringField
    , intField
    , maybeIntField
    , doubleField
    , maybeDoubleField
    , dayField
    , maybeDayField
    , jqueryDayField
    , maybeJqueryDayField
    , timeField
    , maybeTimeField
    , htmlField
    , maybeHtmlField
    , nicHtmlField
    , maybeNicHtmlField
    , selectField
    , maybeSelectField
    , boolField
    , jqueryAutocompleteField
    , maybeJqueryAutocompleteField
    , emailField
    , maybeEmailField
      -- * Pre-built inputs
    , stringInput
    , maybeStringInput
    , boolInput
    , dayInput
    , maybeDayInput
    , emailInput
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
import qualified Text.Email.Validate as Email

-- | A form can produce three different results: there was no data available,
-- the data was invalid, or there was a successful parse.
--
-- The 'Applicative' instance will concatenate the failure messages in two
-- 'FormResult's.
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

-- | The encoding type required by a form. The 'Show' instance produces values
-- that can be inserted directly into HTML.
data Enctype = UrlEncoded | Multipart
instance Show Enctype where
    show UrlEncoded = "application/x-www-form-urlencoded"
    show Multipart = "multipart/form-data"
instance Monoid Enctype where
    mempty = UrlEncoded
    mappend UrlEncoded UrlEncoded = UrlEncoded
    mappend _ _ = Multipart

-- | A generic form, allowing you to specifying the subsite datatype, master
-- site datatype, a datatype for the form XML and the return type.
newtype GForm sub y xml a = GForm
    { deform :: Env -> FileEnv -> StateT Int (GHandler sub y) (FormResult a, xml, Enctype)
    }
type Form sub y = GForm sub y (GWidget sub y ())
type Formlet sub y a = Maybe a -> Form sub y a
type FormField sub y = GForm sub y [FieldInfo sub y]
type FormletField sub y a = Maybe a -> FormField sub y a
type FormInput sub y = GForm sub y [GWidget sub y ()]

-- | Convert the XML in a 'GForm'.
mapFormXml :: (xml1 -> xml2) -> GForm s y xml1 a -> GForm s y xml2 a
mapFormXml f (GForm g) = GForm $ \e fe -> do
    (res, xml, enc) <- g e fe
    return (res, f xml, enc)

-- | Using this as the intermediate XML representation for fields allows us to
-- write generic field functions and then different functions for producing
-- actual HTML. See, for example, 'fieldsToTable' and 'fieldsToPlain'.
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

-- | Display only the actual input widget code, without any decoration.
fieldsToPlain :: [FieldInfo sub y] -> GWidget sub y ()
fieldsToPlain = mapM_ fiInput

fieldsToInput :: [FieldInfo sub y] -> [GWidget sub y ()]
fieldsToInput = map fiInput

-- | Display the label, tooltip, input code and errors in a single row of a
-- table.
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

-- | Create a required field (ie, one that cannot be blank) from a
-- 'FieldProfile'.
requiredFieldHelper :: FieldProfile sub y a -> Maybe a -> FormField sub y a
requiredFieldHelper (FieldProfile parse render mkXml w name' label tooltip) orig =
  GForm $ \env _ -> do
    name <- maybe newFormIdent return name'
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

-- | Create an optional field (ie, one that can be blank) from a
-- 'FieldProfile'.
optionalFieldHelper :: FieldProfile sub y a -> Maybe (Maybe a)
                    -> FormField sub y (Maybe a)
optionalFieldHelper (FieldProfile parse render mkXml w name' label tooltip) orig' =
  GForm $ \env _ -> do
    let orig = join orig'
    name <- maybe newFormIdent return name'
    let (res, val) =
            if null env
                then (FormSuccess Nothing, maybe "" render orig)
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

-- | A generic definition of a form field that can be used for generating both
-- required and optional fields. See 'requiredFieldHelper and
-- 'optionalFieldHelper'.
data FieldProfile sub y a = FieldProfile
    { fpParse :: String -> Either String a
    , fpRender :: a -> String
    , fpHamlet :: Html () -> Html () -> Bool -> Hamlet (Route y)
    , fpWidget :: String -> GWidget sub y ()
    , fpName :: Maybe String
    , fpLabel :: Html ()
    , fpTooltip :: Html ()
    }

--------------------- Begin prebuilt forms

stringField :: Html () -> Html () -> FormletField sub y String
stringField label tooltip = requiredFieldHelper stringFieldProfile
    { fpLabel = label
    , fpTooltip = tooltip
    }

maybeStringField :: Html () -> Html () -> FormletField sub y (Maybe String)
maybeStringField label tooltip = optionalFieldHelper stringFieldProfile
    { fpLabel = label
    , fpTooltip = tooltip
    }

stringFieldProfile :: FieldProfile sub y String
stringFieldProfile = FieldProfile
    { fpParse = Right
    , fpRender = id
    , fpHamlet = \name val isReq -> [$hamlet|
%input#$name$!name=$name$!type=text!:isReq:required!value=$val$
|]
    , fpWidget = \_name -> return ()
    , fpName = Nothing
    , fpLabel = mempty
    , fpTooltip = mempty
    }
instance ToFormField String where
    toFormField = stringField
instance ToFormField (Maybe String) where
    toFormField = maybeStringField

intField :: Integral i => Html () -> Html () -> FormletField sub y i
intField l t = requiredFieldHelper intFieldProfile
    { fpLabel = l
    , fpTooltip = t
    }

maybeIntField :: Integral i =>
              Html () -> Html () -> FormletField sub y (Maybe i)
maybeIntField l t = optionalFieldHelper intFieldProfile
    { fpLabel = l
    , fpTooltip = t
    }

intFieldProfile :: Integral i => FieldProfile sub y i
intFieldProfile = FieldProfile
    { fpParse = maybe (Left "Invalid integer") Right . readMayI
    , fpRender = showI
    , fpHamlet = \name val isReq -> [$hamlet|
%input#$name$!name=$name$!type=number!:isReq:required!value=$val$
|]
    , fpWidget = \_name -> return ()
    , fpName = Nothing
    , fpLabel = mempty
    , fpTooltip = mempty
    }
  where
    showI x = show (fromIntegral x :: Integer)
    readMayI s = case reads s of
                    (x, _):_ -> Just $ fromInteger x
                    [] -> Nothing
instance ToFormField Int where
    toFormField = intField
instance ToFormField (Maybe Int) where
    toFormField = maybeIntField
instance ToFormField Int64 where
    toFormField = intField
instance ToFormField (Maybe Int64) where
    toFormField = maybeIntField

doubleField :: Html () -> Html () -> FormletField sub y Double
doubleField l t = requiredFieldHelper doubleFieldProfile
    { fpLabel = l
    , fpTooltip = t
    }

maybeDoubleField :: Html () -> Html () -> FormletField sub y (Maybe Double)
maybeDoubleField l t = optionalFieldHelper doubleFieldProfile
    { fpLabel = l
    , fpTooltip = t
    }

doubleFieldProfile :: FieldProfile sub y Double
doubleFieldProfile = FieldProfile
    { fpParse = maybe (Left "Invalid number") Right . readMay
    , fpRender = show
    , fpHamlet = \name val isReq -> [$hamlet|
%input#$name$!name=$name$!type=number!:isReq:required!value=$val$
|]
    , fpWidget = \_name -> return ()
    , fpName = Nothing
    , fpLabel = mempty
    , fpTooltip = mempty
    }
instance ToFormField Double where
    toFormField = doubleField
instance ToFormField (Maybe Double) where
    toFormField = maybeDoubleField

dayField :: Html () -> Html () -> FormletField sub y Day
dayField l t = requiredFieldHelper dayFieldProfile
    { fpLabel = l
    , fpTooltip = t
    }

maybeDayField :: Html () -> Html () -> FormletField sub y (Maybe Day)
maybeDayField l t = optionalFieldHelper dayFieldProfile
    { fpLabel = l
    , fpTooltip = t
    }

dayFieldProfile :: FieldProfile sub y Day
dayFieldProfile = FieldProfile
    { fpParse = maybe (Left "Invalid day, must be in YYYY-MM-DD format") Right
              . readMay
    , fpRender = show
    , fpHamlet = \name val isReq -> [$hamlet|
%input#$name$!name=$name$!type=date!:isReq:required!value=$val$
|]
    , fpWidget = const $ return ()
    , fpName = Nothing
    , fpLabel = mempty
    , fpTooltip = mempty
    }
instance ToFormField Day where
    toFormField = dayField
instance ToFormField (Maybe Day) where
    toFormField = maybeDayField

jqueryDayField :: Html () -> Html () -> FormletField sub y Day
jqueryDayField l t = requiredFieldHelper jqueryDayFieldProfile
    { fpLabel = l
    , fpTooltip = t
    }

maybeJqueryDayField :: Html () -> Html () -> FormletField sub y (Maybe Day)
maybeJqueryDayField l t = optionalFieldHelper jqueryDayFieldProfile
    { fpLabel = l
    , fpTooltip = t
    }

jqueryDayFieldProfile :: FieldProfile sub y Day
jqueryDayFieldProfile = FieldProfile
    { fpParse = maybe
                  (Left "Invalid day, must be in YYYY-MM-DD format")
                  Right
              . readMay
    , fpRender = show
    , fpHamlet = \name val isReq -> [$hamlet|
%input#$name$!name=$name$!type=date!:isReq:required!value=$val$
|]
    , fpWidget = \name -> do
        addScriptRemote urlJqueryJs
        addScriptRemote urlJqueryUiJs
        addStylesheetRemote urlJqueryUiCss
        addJavaScript [$hamlet|
$$(function(){$$("#$name$").datepicker({dateFormat:'yy-mm-dd'})});
|]
    , fpName = Nothing
    , fpLabel = mempty
    , fpTooltip = mempty
    }

-- | A newtype wrapper around 'Day', using jQuery UI date picker for the
-- 'ToFormField' instance.
newtype JqueryDay = JqueryDay { unJqueryDay :: Day }
    deriving PersistField
instance Show JqueryDay where
    show = show . unJqueryDay
instance Read JqueryDay where
    readsPrec i s = let [(day, str)] = readsPrec i s :: [(Day, String)]
                    in [((JqueryDay day), str)]
instance Eq JqueryDay where
    x == y = (unJqueryDay x) == (unJqueryDay y)

instance ToFormField JqueryDay where
    toFormField = applyFormTypeWrappers JqueryDay unJqueryDay jqueryDayField
instance ToFormField (Maybe JqueryDay) where
    toFormField = applyFormTypeWrappers (fmap JqueryDay) (fmap unJqueryDay)
                  maybeJqueryDayField

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
timeField label tooltip = requiredFieldHelper timeFieldProfile
    { fpLabel = label
    , fpTooltip = tooltip
    }

maybeTimeField :: Html () -> Html () -> FormletField sub y (Maybe TimeOfDay)
maybeTimeField label tooltip = optionalFieldHelper timeFieldProfile
    { fpLabel = label
    , fpTooltip = tooltip
    }

timeFieldProfile :: FieldProfile sub y TimeOfDay
timeFieldProfile = FieldProfile
    { fpParse = parseTime
    , fpRender = show
    , fpHamlet = \name val isReq -> [$hamlet|
%input#$name$!name=$name$!:isReq:required!value=$val$
|]
    , fpWidget = const $ return ()
    , fpName = Nothing
    , fpLabel = mempty
    , fpTooltip = mempty
    }
instance ToFormField TimeOfDay where
    toFormField = timeField
instance ToFormField (Maybe TimeOfDay) where
    toFormField = maybeTimeField

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
htmlField label tooltip = requiredFieldHelper htmlFieldProfile
    { fpLabel = label
    , fpTooltip = tooltip
    }

maybeHtmlField :: Html () -> Html () -> FormletField sub y (Maybe (Html ()))
maybeHtmlField label tooltip = optionalFieldHelper htmlFieldProfile
    { fpLabel = label
    , fpTooltip = tooltip
    }

htmlFieldProfile :: FieldProfile sub y (Html ())
htmlFieldProfile = FieldProfile
    { fpParse = Right . preEscapedString
    , fpRender = U.toString . renderHtml
    , fpHamlet = \name val _isReq -> [$hamlet|
%textarea.html#$name$!name=$name$ $val$
|]
    , fpWidget = const $ return ()
    , fpName = Nothing
    , fpLabel = mempty
    , fpTooltip = mempty
    }
instance ToFormField (Html ()) where
    toFormField = htmlField
instance ToFormField (Maybe (Html ())) where
    toFormField = maybeHtmlField

newtype NicHtml = NicHtml { unNicHtml :: Html () }
    deriving PersistField

type Html' = Html ()

nicHtmlField :: Html () -> Html () -> FormletField sub y (Html ())
nicHtmlField label tooltip = requiredFieldHelper nicHtmlFieldProfile
    { fpLabel = label
    , fpTooltip = tooltip
    }

maybeNicHtmlField :: Html () -> Html () -> FormletField sub y (Maybe (Html ()))
maybeNicHtmlField label tooltip = optionalFieldHelper nicHtmlFieldProfile
    { fpLabel = label
    , fpTooltip = tooltip
    }

nicHtmlFieldProfile :: FieldProfile sub y (Html ())
nicHtmlFieldProfile = FieldProfile
    { fpParse = Right . preEscapedString
    , fpRender = U.toString . renderHtml
    , fpHamlet = \name val _isReq -> [$hamlet|
%textarea.html#$name$!name=$name$ $val$
|]
    , fpWidget = \name -> do
        addScriptRemote "http://js.nicedit.com/nicEdit-latest.js"
        addJavaScript [$hamlet|bkLib.onDomLoaded(function(){new nicEditor({fullPanel:true}).panelInstance("$name$")});|]
    , fpName = Nothing
    , fpLabel = mempty
    , fpTooltip = mempty
    }
instance ToFormField NicHtml where
    toFormField = applyFormTypeWrappers NicHtml unNicHtml nicHtmlField
instance ToFormField (Maybe NicHtml) where
    toFormField = applyFormTypeWrappers (fmap NicHtml) (fmap unNicHtml)
                  maybeNicHtmlField

applyFormTypeWrappers :: (a -> b) -> (b -> a)
                      -> (f -> g -> FormletField s y a)
                      -> (f -> g -> FormletField s y b)
applyFormTypeWrappers wrap unwrap field l t orig =
    fmap wrap $ field l t $ fmap unwrap orig

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

stringInput :: String -> FormInput sub master String
stringInput n =
    mapFormXml fieldsToInput $
    requiredFieldHelper stringFieldProfile
    { fpName = Just n
    } Nothing

maybeStringInput :: String -> FormInput sub master (Maybe String)
maybeStringInput n =
    mapFormXml fieldsToInput $
    optionalFieldHelper stringFieldProfile
    { fpName = Just n
    } Nothing

boolInput :: String -> FormInput sub master Bool
boolInput n = GForm $ \env _ -> return
    (FormSuccess $ isJust $ lookup n env, return $ addBody [$hamlet|
%input#$n$!type=checkbox!name=$n$
|], UrlEncoded)

dayInput :: String -> FormInput sub master Day
dayInput n =
    mapFormXml fieldsToInput $
    requiredFieldHelper dayFieldProfile
    { fpName = Just n
    } Nothing

maybeDayInput :: String -> FormInput sub master (Maybe Day)
maybeDayInput n =
    mapFormXml fieldsToInput $
    optionalFieldHelper dayFieldProfile
    { fpName = Just n
    } Nothing

--------------------- End prebuilt inputs

-- | Get a unique identifier.
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
runFormGet' :: GForm sub y xml a -> GHandler sub y a
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

-- | This function allows two different monadic functions to share the same
-- input and have their results concatenated. This is particularly useful for
-- allowing 'mkToForm' to share its input with mkPersist.
share2 :: Monad m => (a -> m [b]) -> (a -> m [b]) -> a -> m [b]
share2 f g a = do
    f' <- f a
    g' <- g a
    return $ f' ++ g'

-- | Create 'ToForm' instances for the entities given. In addition to regular 'EntityDef' attributes understood by persistent, it also understands label= and tooltip=.
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

jqueryAutocompleteField ::
    Route y -> Html () -> Html () -> FormletField sub y String
jqueryAutocompleteField src l t =
    requiredFieldHelper $ (jqueryAutocompleteFieldProfile src)
        { fpLabel = l
        , fpTooltip = t
        }

maybeJqueryAutocompleteField ::
    Route y -> Html () -> Html () -> FormletField sub y (Maybe String)
maybeJqueryAutocompleteField src l t =
    optionalFieldHelper $ (jqueryAutocompleteFieldProfile src)
        { fpLabel = l
        , fpTooltip = t
        }

jqueryAutocompleteFieldProfile :: Route y -> FieldProfile sub y String
jqueryAutocompleteFieldProfile src = FieldProfile
    { fpParse = Right
    , fpRender = id
    , fpHamlet = \name val isReq -> [$hamlet|
%input.autocomplete#$name$!name=$name$!type=text!:isReq:required!value=$val$
|]
    , fpWidget = \name -> do
        addScriptRemote urlJqueryJs
        addScriptRemote urlJqueryUiJs
        addStylesheetRemote urlJqueryUiCss
        addJavaScript [$hamlet|
$$(function(){$$("#$name$").autocomplete({source:"@src@",minLength:2})});
|]
    , fpName = Nothing
    , fpLabel = mempty
    , fpTooltip = mempty
    }

emailFieldProfile :: FieldProfile s y String
emailFieldProfile = FieldProfile
    { fpParse = \s -> if Email.isValid s
                        then Right s
                        else Left "Invalid e-mail address"
    , fpRender = id
    , fpHamlet = \name val isReq -> [$hamlet|
%input#$name$!name=$name$!type=email!:isReq:required!value=$val$
|]
    , fpWidget = const $ return ()
    , fpName = Nothing
    , fpLabel = mempty
    , fpTooltip = mempty
    }

emailField :: Html () -> Html () -> FormletField sub y String
emailField label tooltip = requiredFieldHelper emailFieldProfile
    { fpLabel = label
    , fpTooltip = tooltip
    }

maybeEmailField :: Html () -> Html () -> FormletField sub y (Maybe String)
maybeEmailField label tooltip = optionalFieldHelper emailFieldProfile
    { fpLabel = label
    , fpTooltip = tooltip
    }

emailInput :: String -> FormInput sub master String
emailInput n =
    mapFormXml fieldsToInput $
    requiredFieldHelper emailFieldProfile
    { fpName = Just n
    } Nothing
