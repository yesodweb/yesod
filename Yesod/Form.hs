{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
    , FormFieldSettings (..)
    , labelSettings
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
    , jqueryDayTimeField
    , jqueryDayTimeFieldProfile
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
    , intInput
    , boolInput
    , dayInput
    , maybeDayInput
    , emailInput
      -- * Template Haskell
    , mkToForm
    ) where

import Text.Hamlet
import Yesod.Request
import Yesod.Handler
import Control.Applicative hiding (optional)
import Data.Time (UTCTime(..), Day, TimeOfDay(..))
import Data.Time.LocalTime (timeOfDayToTime, timeToTimeOfDay)
import Data.Maybe (fromMaybe, mapMaybe)
import "transformers" Control.Monad.IO.Class
import Control.Monad ((<=<), liftM, join)
import Data.Monoid (Monoid (..))
import Control.Monad.Trans.State
import Language.Haskell.TH.Syntax
import Database.Persist.Base (EntityDef (..))
import Data.Char (toUpper, isUpper)
import Data.Int (Int64)
import qualified Data.ByteString.Lazy.UTF8 as U
import Yesod.Widget
import Control.Arrow ((&&&))
import qualified Text.Email.Validate as Email
import Data.Char (isSpace)
import Yesod.Yesod (Yesod (..))
import Data.List (group, sort)

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
    , fiName :: String
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

class ToForm a y where
    toForm :: Maybe a -> Form sub y a
class ToFormField a y where
    toFormField :: FormFieldSettings -> Maybe a -> FormField sub y a

data FormFieldSettings = FormFieldSettings
    { ffsLabel :: Html ()
    , ffsTooltip :: Html ()
    , ffsId :: Maybe String
    , ffsName :: Maybe String
    }

-- | Create a required field (ie, one that cannot be blank) from a
-- 'FieldProfile'.ngs
requiredFieldHelper :: FieldProfile sub y a -> FormFieldSettings -> Maybe a -> FormField sub y a
requiredFieldHelper
    (FieldProfile parse render mkXml w)
    (FormFieldSettings label tooltip theId' name') orig =
  GForm $ \env _ -> do
    name <- maybe newFormIdent return name'
    theId <- maybe newFormIdent return theId'
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
            , fiIdent = theId
            , fiName = name
            , fiInput = w theId >> addBody (mkXml theId name val True)
            , fiErrors = case res of
                            FormFailure [x] -> Just $ string x
                            _ -> Nothing
            }
    return (res, [fi], UrlEncoded)

-- | Create an optional field (ie, one that can be blank) from a
-- 'FieldProfile'.
optionalFieldHelper :: FieldProfile sub y a -> FormFieldSettings -> FormletField sub y (Maybe a)
optionalFieldHelper
 (FieldProfile parse render mkXml w)
 (FormFieldSettings label tooltip theId' name') orig' =
  GForm $ \env _ -> do
    let orig = join orig'
    name <- maybe newFormIdent return name'
    theId <- maybe newFormIdent return theId'
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
            , fiIdent = theId
            , fiName = name
            , fiInput = w theId >> addBody (mkXml theId name val False)
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
    , fpHamlet :: String -> String -> String -> Bool -> Hamlet (Route y)
    , fpWidget :: String -> GWidget sub y ()
    }

--------------------- Begin prebuilt forms

stringField :: FormFieldSettings -> FormletField sub y String
stringField = requiredFieldHelper stringFieldProfile

maybeStringField :: FormFieldSettings -> FormletField sub y (Maybe String)
maybeStringField = optionalFieldHelper stringFieldProfile

stringFieldProfile :: FieldProfile sub y String
stringFieldProfile = FieldProfile
    { fpParse = Right
    , fpRender = id
    , fpHamlet = \theId name val isReq -> [$hamlet|
%input#$theId$!name=$name$!type=text!:isReq:required!value=$val$
|]
    , fpWidget = \_name -> return ()
    }
instance ToFormField String y where
    toFormField = stringField
instance ToFormField (Maybe String) y where
    toFormField = maybeStringField

intInput :: Integral i => String -> FormInput sub master i
intInput n =
    mapFormXml fieldsToInput $
    requiredFieldHelper intFieldProfile (nameSettings n) Nothing

intField :: Integral i => FormFieldSettings -> FormletField sub y i
intField = requiredFieldHelper intFieldProfile

maybeIntField :: Integral i => FormFieldSettings -> FormletField sub y (Maybe i)
maybeIntField = optionalFieldHelper intFieldProfile

intFieldProfile :: Integral i => FieldProfile sub y i
intFieldProfile = FieldProfile
    { fpParse = maybe (Left "Invalid integer") Right . readMayI
    , fpRender = showI
    , fpHamlet = \theId name val isReq -> [$hamlet|
%input#$theId$!name=$name$!type=number!:isReq:required!value=$val$
|]
    , fpWidget = \_name -> return ()
    }
  where
    showI x = show (fromIntegral x :: Integer)
    readMayI s = case reads s of
                    (x, _):_ -> Just $ fromInteger x
                    [] -> Nothing
instance ToFormField Int y where
    toFormField = intField
instance ToFormField (Maybe Int) y where
    toFormField = maybeIntField
instance ToFormField Int64 y where
    toFormField = intField
instance ToFormField (Maybe Int64) y where
    toFormField = maybeIntField

doubleField :: FormFieldSettings -> FormletField sub y Double
doubleField = requiredFieldHelper doubleFieldProfile

maybeDoubleField :: FormFieldSettings -> FormletField sub y (Maybe Double)
maybeDoubleField = optionalFieldHelper doubleFieldProfile

doubleFieldProfile :: FieldProfile sub y Double
doubleFieldProfile = FieldProfile
    { fpParse = maybe (Left "Invalid number") Right . readMay
    , fpRender = show
    , fpHamlet = \theId name val isReq -> [$hamlet|
%input#$theId$!name=$name$!type=number!:isReq:required!value=$val$
|]
    , fpWidget = \_name -> return ()
    }
instance ToFormField Double y where
    toFormField = doubleField
instance ToFormField (Maybe Double) y where
    toFormField = maybeDoubleField

dayField :: FormFieldSettings -> FormletField sub y Day
dayField = requiredFieldHelper dayFieldProfile

maybeDayField :: FormFieldSettings -> FormletField sub y (Maybe Day)
maybeDayField = optionalFieldHelper dayFieldProfile

dayFieldProfile :: FieldProfile sub y Day
dayFieldProfile = FieldProfile
    { fpParse = parseDate
    , fpRender = show
    , fpHamlet = \theId name val isReq -> [$hamlet|
%input#$theId$!name=$name$!type=date!:isReq:required!value=$val$
|]
    , fpWidget = const $ return ()
    }
instance ToFormField Day y where
    toFormField = dayField
instance ToFormField (Maybe Day) y where
    toFormField = maybeDayField

jqueryDayField :: Yesod y => FormFieldSettings -> FormletField sub y Day
jqueryDayField = requiredFieldHelper jqueryDayFieldProfile

maybeJqueryDayField :: Yesod y => FormFieldSettings -> FormletField sub y (Maybe Day)
maybeJqueryDayField = optionalFieldHelper jqueryDayFieldProfile

jqueryDayFieldProfile :: Yesod y => FieldProfile sub y Day
jqueryDayFieldProfile = FieldProfile
    { fpParse = maybe
                  (Left "Invalid day, must be in YYYY-MM-DD format")
                  Right
              . readMay
    , fpRender = show
    , fpHamlet = \theId name val isReq -> [$hamlet|
%input#$theId$!name=$name$!type=date!:isReq:required!value=$val$
|]
    , fpWidget = \name -> do
        addScript' urlJqueryJs
        addScript' urlJqueryUiJs
        addStylesheet' urlJqueryUiCss
        addJavaScript [$hamlet|
$$(function(){$$("#$name$").datepicker({dateFormat:'yy-mm-dd'})});
|]
    }

-- | Replaces all instances of a value in a list by another value.
-- from http://hackage.haskell.org/packages/archive/cgi/3001.1.7.1/doc/html/src/Network-CGI-Protocol.html#replace
replace :: Eq a => a -> a -> [a] -> [a]
replace x y = map (\z -> if z == x then y else z)

ifRight :: Either a b -> (b -> c) -> Either a c
ifRight e f = case e of
            Left l  -> Left l
            Right r -> Right $ f r

showLeadingZero :: (Show a) => a -> String
showLeadingZero time = let t = show time in if length t == 1 then "0" ++ t else t

parseUTCTime :: String -> Either String UTCTime
parseUTCTime s =
    let (dateS, timeS) = break isSpace (dropWhile isSpace s)
    in let dateE = (parseDate dateS)
       in case dateE of
            Left l -> Left l
            Right date -> ifRight (parseTime timeS)
                                  (\time -> UTCTime date (timeOfDayToTime time))

jqueryDayTimeField :: Yesod y => FormFieldSettings -> FormletField sub y UTCTime
jqueryDayTimeField = requiredFieldHelper jqueryDayTimeFieldProfile

parseDate :: String -> Either String Day
parseDate = maybe (Left "Invalid day, must be in YYYY-MM-DD format") Right
              . readMay . replace '/' '-'


-- use A.M/P.M and drop seconds and "UTC" (as opposed to normal UTCTime show)
jqueryDayTimeUTCTime :: UTCTime -> String
jqueryDayTimeUTCTime (UTCTime day utcTime) =
  let timeOfDay = timeToTimeOfDay utcTime
  in (replace '-' '/' (show day)) ++ " " ++ showTimeOfDay timeOfDay
  where
    showTimeOfDay (TimeOfDay hour minute _) =
      let (h, apm) = if hour < 12 then (hour, "AM") else (hour - 12, "PM")
      in (show h) ++ ":" ++ (showLeadingZero minute) ++ " " ++ apm

jqueryDayTimeFieldProfile :: Yesod y => FieldProfile sub y UTCTime
jqueryDayTimeFieldProfile = FieldProfile
    { fpParse  = parseUTCTime
    , fpRender = jqueryDayTimeUTCTime
    , fpHamlet = \theId name val isReq -> [$hamlet|
%input#$theId$!name=$name$!type=date!:isReq:required!value=$val$
|]
    , fpWidget = \name -> do
        addScript' urlJqueryJs
        addScript' urlJqueryUiJs
        addScript' urlJqueryUiDateTimePicker
        addStylesheet' urlJqueryUiCss
        addJavaScript [$hamlet|
$$(function(){$$("#$name$").datetimepicker({dateFormat : "yyyy/mm/dd h:MM TT"})});
|]
    }

parseTime :: String -> Either String TimeOfDay
parseTime (h2:':':m1:m2:[]) = parseTimeHelper ('0', h2, m1, m2, '0', '0')
parseTime (h1:h2:':':m1:m2:[]) = parseTimeHelper (h1, h2, m1, m2, '0', '0')
parseTime (h1:h2:':':m1:m2:' ':'A':'M':[]) =
    parseTimeHelper (h1, h2, m1, m2, '0', '0')
parseTime (h1:h2:':':m1:m2:' ':'P':'M':[]) = 
    let [h1', h2'] = show $ (read [h1, h2] :: Int) + 12
    in parseTimeHelper (h1', h2', m1, m2, '0', '0')
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

timeField :: FormFieldSettings -> FormletField sub y TimeOfDay
timeField = requiredFieldHelper timeFieldProfile

maybeTimeField :: FormFieldSettings -> FormletField sub y (Maybe TimeOfDay)
maybeTimeField = optionalFieldHelper timeFieldProfile

timeFieldProfile :: FieldProfile sub y TimeOfDay
timeFieldProfile = FieldProfile
    { fpParse = parseTime
    , fpRender = show
    , fpHamlet = \theId name val isReq -> [$hamlet|
%input#$theId$!name=$name$!:isReq:required!value=$val$
|]
    , fpWidget = const $ return ()
    }
instance ToFormField TimeOfDay y where
    toFormField = timeField
instance ToFormField (Maybe TimeOfDay) y where
    toFormField = maybeTimeField

boolField :: FormFieldSettings -> Maybe Bool -> FormField sub y Bool
boolField ffs orig = GForm $ \env _ -> do
    let label = ffsLabel ffs
        tooltip = ffsTooltip ffs
    name <- maybe newFormIdent return $ ffsName ffs
    theId <- maybe newFormIdent return $ ffsId ffs
    let (res, val) =
            if null env
                then (FormMissing, fromMaybe False orig)
                else case lookup name env of
                        Nothing -> (FormSuccess False, False)
                        Just _ -> (FormSuccess True, True)
    let fi = FieldInfo
            { fiLabel = label
            , fiTooltip = tooltip
            , fiIdent = theId
            , fiName = name
            , fiInput = addBody [$hamlet|
%input#$theId$!type=checkbox!name=$name$!:val:checked
|]
            , fiErrors = case res of
                            FormFailure [x] -> Just $ string x
                            _ -> Nothing
            }
    return (res, [fi], UrlEncoded)
instance ToFormField Bool y where
    toFormField = boolField

htmlField :: FormFieldSettings -> FormletField sub y (Html ())
htmlField = requiredFieldHelper htmlFieldProfile

maybeHtmlField :: FormFieldSettings -> FormletField sub y (Maybe (Html ()))
maybeHtmlField = optionalFieldHelper htmlFieldProfile

htmlFieldProfile :: FieldProfile sub y (Html ())
htmlFieldProfile = FieldProfile
    { fpParse = Right . preEscapedString
    , fpRender = U.toString . renderHtml
    , fpHamlet = \theId name val _isReq -> [$hamlet|
%textarea.html#$theId$!name=$name$ $val$
|]
    , fpWidget = const $ return ()
    }
instance ToFormField (Html ()) y where
    toFormField = htmlField
instance ToFormField (Maybe (Html ())) y where
    toFormField = maybeHtmlField

type Html' = Html ()

nicHtmlField :: Yesod y => FormFieldSettings -> FormletField sub y (Html ())
nicHtmlField = requiredFieldHelper nicHtmlFieldProfile

maybeNicHtmlField :: Yesod y => FormFieldSettings -> FormletField sub y (Maybe (Html ()))
maybeNicHtmlField = optionalFieldHelper nicHtmlFieldProfile

nicHtmlFieldProfile :: Yesod y => FieldProfile sub y (Html ())
nicHtmlFieldProfile = FieldProfile
    { fpParse = Right . preEscapedString
    , fpRender = U.toString . renderHtml
    , fpHamlet = \theId name val _isReq -> [$hamlet|
%textarea.html#$theId$!name=$name$ $val$
|]
    , fpWidget = \name -> do
        addScript' urlNicEdit
        addJavaScript [$hamlet|bkLib.onDomLoaded(function(){new nicEditor({fullPanel:true}).panelInstance("$name$")});|]
    }

readMay :: Read a => String -> Maybe a
readMay s = case reads s of
                (x, _):_ -> Just x
                [] -> Nothing

selectField :: Eq x => [(x, String)]
            -> FormFieldSettings
            -> Maybe x -> FormField sub master x
selectField pairs ffs initial = GForm $ \env _ -> do
    let label = ffsLabel ffs
        tooltip = ffsTooltip ffs
    theId <- maybe newFormIdent return $ ffsId ffs
    name <- maybe newFormIdent return $ ffsName ffs
    let pairs' = zip [1 :: Int ..] pairs
    let res = case lookup name env of
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
%select#$theId$!name=$name$
    %option!value=none
    $forall pairs' pair
        %option!value=$show.fst.pair$!:isSelected.fst.snd.pair:selected $snd.snd.pair$
|]
    let fi = FieldInfo
            { fiLabel = label
            , fiTooltip = tooltip
            , fiIdent = theId
            , fiName = name
            , fiInput = addBody input
            , fiErrors = case res of
                            FormFailure [x] -> Just $ string x
                            _ -> Nothing
            }
    return (res, [fi], UrlEncoded)

maybeSelectField :: Eq x => [(x, String)]
                 -> FormFieldSettings
                 -> FormletField sub master (Maybe x)
maybeSelectField pairs ffs initial' = GForm $ \env _ -> do
    let initial = join initial'
        label = ffsLabel ffs
        tooltip = ffsTooltip ffs
    theId <- maybe newFormIdent return $ ffsId ffs
    name <- maybe newFormIdent return $ ffsName ffs
    let pairs' = zip [1 :: Int ..] pairs
    let res = case lookup name env of
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
%select#$theId$!name=$name$
    %option!value=none
    $forall pairs' pair
        %option!value=$show.fst.pair$!:isSelected.fst.snd.pair:selected $snd.snd.pair$
|]
    let fi = FieldInfo
            { fiLabel = label
            , fiTooltip = tooltip
            , fiIdent = theId
            , fiName = name
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
    requiredFieldHelper stringFieldProfile (nameSettings n) Nothing

maybeStringInput :: String -> FormInput sub master (Maybe String)
maybeStringInput n =
    mapFormXml fieldsToInput $
    optionalFieldHelper stringFieldProfile (nameSettings n) Nothing

boolInput :: String -> FormInput sub master Bool
boolInput n = GForm $ \env _ -> return
    (FormSuccess $ fromMaybe "" (lookup n env) /= "", return $ addBody [$hamlet|
%input#$n$!type=checkbox!name=$n$
|], UrlEncoded)

dayInput :: String -> FormInput sub master Day
dayInput n =
    mapFormXml fieldsToInput $
    requiredFieldHelper dayFieldProfile (nameSettings n) Nothing

maybeDayInput :: String -> FormInput sub master (Maybe Day)
maybeDayInput n =
    mapFormXml fieldsToInput $
    optionalFieldHelper dayFieldProfile (nameSettings n) Nothing

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

-- | Create 'ToForm' instances for the entities given. In addition to regular 'EntityDef' attributes understood by persistent, it also understands label= and tooltip=.
mkToForm :: [EntityDef] -> Q [Dec]
mkToForm = mapM derive
  where
    afterPeriod s =
        case dropWhile (/= '.') s of
            ('.':t) -> t
            _ -> s
    beforePeriod s =
        case break (== '.') s of
            (t, '.':_) -> Just t
            _ -> Nothing
    getSuperclass (_, _, z) = getTFF' z >>= beforePeriod
    getTFF (_, _, z) = maybe "toFormField" afterPeriod $ getTFF' z
    getTFF' [] = Nothing
    getTFF' (('t':'o':'F':'o':'r':'m':'F':'i':'e':'l':'d':'=':x):_) = Just x
    getTFF' (_:x) = getTFF' x
    getLabel (x, _, z) = fromMaybe (toLabel x) $ getLabel' z
    getLabel' [] = Nothing
    getLabel' (('l':'a':'b':'e':'l':'=':x):_) = Just x
    getLabel' (_:x) = getLabel' x
    getTooltip (_, _, z) = fromMaybe "" $ getTooltip' z
    getTooltip' (('t':'o':'o':'l':'t':'i':'p':'=':x):_) = Just x
    getTooltip' (_:x) = getTooltip' x
    getTooltip' [] = Nothing
    getId (_, _, z) = fromMaybe "" $ getId' z
    getId' (('i':'d':'=':x):_) = Just x
    getId' (_:x) = getId' x
    getId' [] = Nothing
    getName (_, _, z) = fromMaybe "" $ getName' z
    getName' (('n':'a':'m':'e':'=':x):_) = Just x
    getName' (_:x) = getName' x
    getName' [] = Nothing
    derive :: EntityDef -> Q Dec
    derive t = do
        let cols = map ((getId &&& getName) &&& ((getLabel &&& getTooltip) &&& getTFF)) $ entityColumns t
        ap <- [|(<*>)|]
        just <- [|pure|]
        nothing <- [|Nothing|]
        let just' = just `AppE` ConE (mkName $ entityName t)
        string' <- [|string|]
        mfx <- [|mapFormXml|]
        ftt <- [|fieldsToTable|]
        ffs' <- [|FormFieldSettings|]
        let stm "" = nothing
            stm x = just `AppE` LitE (StringL x)
        let go_ = go ap just' ffs' stm string' mfx ftt
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
        let y = mkName "y"
        let ctx = map (\x -> ClassP (mkName x) [VarT y])
                $ map head $ group $ sort
                $ mapMaybe getSuperclass
                $ entityColumns t
        return $ InstanceD ctx ( ConT ''ToForm
                              `AppT` ConT (mkName $ entityName t)
                              `AppT` VarT y)
            [FunD (mkName "toForm") [c1, c2]]
    go ap just' ffs' stm string' mfx ftt a =
        let x = foldl (ap' ap) just' $ map (go' ffs' stm string') a
         in mfx `AppE` ftt `AppE` x
    go' ffs' stm string' (((theId, name), ((label, tooltip), tff)), ex) =
        let label' = string' `AppE` LitE (StringL label)
            tooltip' = string' `AppE` LitE (StringL tooltip)
            ffs = ffs' `AppE`
                  label' `AppE`
                  tooltip' `AppE`
                  (stm theId) `AppE`
                  (stm name)
         in VarE (mkName tff) `AppE` ffs `AppE` ex
    ap' ap x y = InfixE (Just x) ap (Just y)

toLabel :: String -> String
toLabel "" = ""
toLabel (x:rest) = toUpper x : go rest
  where
    go "" = ""
    go (c:cs)
        | isUpper c = ' ' : c : go cs
        | otherwise = c : go cs

jqueryAutocompleteField :: Yesod y =>
    Route y -> FormFieldSettings -> FormletField sub y String
jqueryAutocompleteField = requiredFieldHelper . jqueryAutocompleteFieldProfile

maybeJqueryAutocompleteField :: Yesod y =>
    Route y -> FormFieldSettings -> FormletField sub y (Maybe String)
maybeJqueryAutocompleteField src =
    optionalFieldHelper $ jqueryAutocompleteFieldProfile src

jqueryAutocompleteFieldProfile :: Yesod y => Route y -> FieldProfile sub y String
jqueryAutocompleteFieldProfile src = FieldProfile
    { fpParse = Right
    , fpRender = id
    , fpHamlet = \theId name val isReq -> [$hamlet|
%input.autocomplete#$theId$!name=$name$!type=text!:isReq:required!value=$val$
|]
    , fpWidget = \name -> do
        addScript' urlJqueryJs
        addScript' urlJqueryUiJs
        addStylesheet' urlJqueryUiCss
        addJavaScript [$hamlet|
$$(function(){$$("#$name$").autocomplete({source:"@src@",minLength:2})});
|]
    }

emailFieldProfile :: FieldProfile s y String
emailFieldProfile = FieldProfile
    { fpParse = \s -> if Email.isValid s
                        then Right s
                        else Left "Invalid e-mail address"
    , fpRender = id
    , fpHamlet = \theId name val isReq -> [$hamlet|
%input#$theId$!name=$name$!type=email!:isReq:required!value=$val$
|]
    , fpWidget = const $ return ()
    }

emailField :: FormFieldSettings -> FormletField sub y String
emailField = requiredFieldHelper emailFieldProfile

maybeEmailField :: FormFieldSettings -> FormletField sub y (Maybe String)
maybeEmailField = optionalFieldHelper emailFieldProfile

emailInput :: String -> FormInput sub master String
emailInput n =
    mapFormXml fieldsToInput $
    requiredFieldHelper emailFieldProfile (nameSettings n) Nothing

nameSettings :: String -> FormFieldSettings
nameSettings n = FormFieldSettings mempty mempty (Just n) (Just n)

addScript' :: (y -> Either (Route y) String) -> GWidget sub y ()
addScript' f = do
    y <- liftHandler getYesod
    addScriptEither $ f y

addStylesheet' :: (y -> Either (Route y) String) -> GWidget sub y ()
addStylesheet' f = do
    y <- liftHandler getYesod
    addStylesheetEither $ f y

labelSettings :: String -> FormFieldSettings
labelSettings l = FormFieldSettings (string l) mempty Nothing Nothing
