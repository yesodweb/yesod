{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Yesod.Form.Fields
    ( -- * Fields
      -- ** Required
      stringField
    , textareaField
    , hiddenField
    , intField
    , doubleField
    , dayField
    , timeField
    , htmlField
    , selectField
    , boolField
    , emailField
    , urlField
    , fileField
      -- ** Optional
    , maybeStringField
    , maybeTextareaField
    , maybeHiddenField
    , maybeIntField
    , maybeDoubleField
    , maybeDayField
    , maybeTimeField
    , maybeHtmlField
    , maybeSelectField
    , maybeEmailField
    , maybeUrlField
    , maybeFileField
      -- * Inputs
      -- ** Required
    , stringInput
    , intInput
    , boolInput
    , dayInput
    , emailInput
    , urlInput
      -- ** Optional
    , maybeStringInput
    , maybeDayInput
    , maybeIntInput
    ) where

import Yesod.Form.Core
import Yesod.Form.Profiles
import Yesod.Request (FileInfo)
import Yesod.Widget (GWidget)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask)
import Data.Time (Day, TimeOfDay)
import Text.Hamlet
import Data.Monoid
import Control.Monad (join)
import Data.Maybe (fromMaybe)

stringField :: (IsForm f, FormType f ~ String)
            => FormFieldSettings -> Maybe String -> f
stringField = requiredFieldHelper stringFieldProfile

maybeStringField :: (IsForm f, FormType f ~ Maybe String)
                 => FormFieldSettings -> Maybe (Maybe String) -> f
maybeStringField = optionalFieldHelper stringFieldProfile

intInput :: Integral i => String -> FormInput sub master i
intInput n =
    mapFormXml fieldsToInput $
    requiredFieldHelper intFieldProfile (nameSettings n) Nothing

maybeIntInput :: Integral i => String -> FormInput sub master (Maybe i)
maybeIntInput n =
    mapFormXml fieldsToInput $
    optionalFieldHelper intFieldProfile (nameSettings n) Nothing

intField :: (Integral (FormType f), IsForm f)
         => FormFieldSettings -> Maybe (FormType f) -> f
intField = requiredFieldHelper intFieldProfile

maybeIntField :: (Integral i, FormType f ~ Maybe i, IsForm f)
              => FormFieldSettings -> Maybe (FormType f) -> f
maybeIntField = optionalFieldHelper intFieldProfile

doubleField :: (IsForm f, FormType f ~ Double)
            => FormFieldSettings -> Maybe Double -> f
doubleField = requiredFieldHelper doubleFieldProfile

maybeDoubleField :: (IsForm f, FormType f ~ Maybe Double)
                 => FormFieldSettings -> Maybe (Maybe Double) -> f
maybeDoubleField = optionalFieldHelper doubleFieldProfile

dayField :: (IsForm f, FormType f ~ Day)
         => FormFieldSettings -> Maybe Day -> f
dayField = requiredFieldHelper dayFieldProfile

maybeDayField :: (IsForm f, FormType f ~ Maybe Day)
              => FormFieldSettings -> Maybe (Maybe Day) -> f
maybeDayField = optionalFieldHelper dayFieldProfile

timeField :: (IsForm f, FormType f ~ TimeOfDay)
          => FormFieldSettings -> Maybe TimeOfDay -> f
timeField = requiredFieldHelper timeFieldProfile

maybeTimeField :: (IsForm f, FormType f ~ Maybe TimeOfDay)
               => FormFieldSettings -> Maybe (Maybe TimeOfDay) -> f
maybeTimeField = optionalFieldHelper timeFieldProfile

boolField :: (IsForm f, FormType f ~ Bool)
          => FormFieldSettings -> Maybe Bool -> f
boolField ffs orig = toForm $ do
    env <- askParams
    let label = ffsLabel ffs
        tooltip = ffsTooltip ffs
    name <- maybe newFormIdent return $ ffsName ffs
    theId <- maybe newFormIdent return $ ffsId ffs
    let (res, val) =
            if null env
                then (FormMissing, fromMaybe False orig)
                else case lookup name env of
                        Nothing -> (FormSuccess False, False)
                        Just "" -> (FormSuccess False, False)
                        Just "false" -> (FormSuccess False, False)
                        Just _ -> (FormSuccess True, True)
    let fi = FieldInfo
            { fiLabel = string label
            , fiTooltip = tooltip
            , fiIdent = theId
            , fiInput = [$hamlet|
%input#$theId$!type=checkbox!name=$name$!:val:checked
|]
            , fiErrors = case res of
                            FormFailure [x] -> Just $ string x
                            _ -> Nothing
            , fiRequired = True
            }
    return (res, fi, UrlEncoded)

htmlField :: (IsForm f, FormType f ~ Html)
          => FormFieldSettings -> Maybe Html -> f
htmlField = requiredFieldHelper htmlFieldProfile

maybeHtmlField :: (IsForm f, FormType f ~ Maybe Html)
               => FormFieldSettings -> Maybe (Maybe Html) -> f
maybeHtmlField = optionalFieldHelper htmlFieldProfile

selectField :: (Eq x, IsForm f, FormType f ~ x)
            => [(x, String)]
            -> FormFieldSettings
            -> Maybe x
            -> f
selectField pairs ffs initial = toForm $ do
    env <- askParams
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
            { fiLabel = string label
            , fiTooltip = tooltip
            , fiIdent = theId
            , fiInput = input
            , fiErrors = case res of
                            FormFailure [x] -> Just $ string x
                            _ -> Nothing
            , fiRequired = True
            }
    return (res, fi, UrlEncoded)

maybeSelectField :: (Eq x, IsForm f, Maybe x ~ FormType f)
                 => [(x, String)]
                 -> FormFieldSettings
                 -> Maybe (FormType f)
                 -> f
maybeSelectField pairs ffs initial' = toForm $ do
    env <- askParams
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
            { fiLabel = string label
            , fiTooltip = tooltip
            , fiIdent = theId
            , fiInput = input
            , fiErrors = case res of
                            FormFailure [x] -> Just $ string x
                            _ -> Nothing
            , fiRequired = False
            }
    return (res, fi, UrlEncoded)

stringInput :: String -> FormInput sub master String
stringInput n =
    mapFormXml fieldsToInput $
    requiredFieldHelper stringFieldProfile (nameSettings n) Nothing

maybeStringInput :: String -> FormInput sub master (Maybe String)
maybeStringInput n =
    mapFormXml fieldsToInput $
    optionalFieldHelper stringFieldProfile (nameSettings n) Nothing

boolInput :: String -> FormInput sub master Bool
boolInput n = GForm $ do
    env <- askParams
    let res = case lookup n env of
                Nothing -> FormSuccess False
                Just "" -> FormSuccess False
                Just "false" -> FormSuccess False
                Just _ -> FormSuccess True
    let xml = [$hamlet|%input#$n$!type=checkbox!name=$n$|]
    return (res, [xml], UrlEncoded)

dayInput :: String -> FormInput sub master Day
dayInput n =
    mapFormXml fieldsToInput $
    requiredFieldHelper dayFieldProfile (nameSettings n) Nothing

maybeDayInput :: String -> FormInput sub master (Maybe Day)
maybeDayInput n =
    mapFormXml fieldsToInput $
    optionalFieldHelper dayFieldProfile (nameSettings n) Nothing

nameSettings :: String -> FormFieldSettings
nameSettings n = FormFieldSettings mempty mempty (Just n) (Just n)

urlField :: (IsForm f, FormType f ~ String)
         => FormFieldSettings -> Maybe String -> f
urlField = requiredFieldHelper urlFieldProfile

maybeUrlField :: (IsForm f, FormType f ~ Maybe String)
               => FormFieldSettings -> Maybe (Maybe String) -> f
maybeUrlField = optionalFieldHelper urlFieldProfile

urlInput :: String -> FormInput sub master String
urlInput n =
    mapFormXml fieldsToInput $
    requiredFieldHelper urlFieldProfile (nameSettings n) Nothing

emailField :: (IsForm f, FormType f ~ String)
           => FormFieldSettings -> Maybe String -> f
emailField = requiredFieldHelper emailFieldProfile

maybeEmailField :: (IsForm f, FormType f ~ Maybe String)
                => FormFieldSettings -> Maybe (Maybe String) -> f
maybeEmailField = optionalFieldHelper emailFieldProfile

emailInput :: String -> FormInput sub master String
emailInput n =
    mapFormXml fieldsToInput $
    requiredFieldHelper emailFieldProfile (nameSettings n) Nothing

textareaField :: (IsForm f, FormType f ~ Textarea)
              => FormFieldSettings -> Maybe Textarea -> f
textareaField = requiredFieldHelper textareaFieldProfile

maybeTextareaField :: FormFieldSettings -> FormletField sub y (Maybe Textarea)
maybeTextareaField = optionalFieldHelper textareaFieldProfile

hiddenField :: (IsForm f, FormType f ~ String)
            => FormFieldSettings -> Maybe String -> f
hiddenField = requiredFieldHelper hiddenFieldProfile

maybeHiddenField :: (IsForm f, FormType f ~ Maybe String)
                 => FormFieldSettings -> Maybe (Maybe String) -> f
maybeHiddenField = optionalFieldHelper hiddenFieldProfile

fileField :: (IsForm f, FormType f ~ FileInfo)
          => FormFieldSettings -> f
fileField ffs = toForm $ do
    env <- lift ask
    fenv <- lift $ lift ask
    let (FormFieldSettings label tooltip theId' name') = ffs
    name <- maybe newFormIdent return name'
    theId <- maybe newFormIdent return theId'
    let res =
            if null env && null fenv
                then FormMissing
                else case lookup name fenv of
                        Nothing -> FormFailure ["File is required"]
                        Just x -> FormSuccess x
    let fi = FieldInfo
            { fiLabel = string label
            , fiTooltip = tooltip
            , fiIdent = theId
            , fiInput = fileWidget theId name True
            , fiErrors = case res of
                            FormFailure [x] -> Just $ string x
                            _ -> Nothing
            , fiRequired = True
            }
    let res' = case res of
                FormFailure [e] -> FormFailure [label ++ ": " ++ e]
                _ -> res
    return (res', fi, Multipart)

maybeFileField :: (IsForm f, FormType f ~ Maybe FileInfo)
               => FormFieldSettings -> f
maybeFileField ffs = toForm $ do
    fenv <- lift $ lift ask
    let (FormFieldSettings label tooltip theId' name') = ffs
    name <- maybe newFormIdent return name'
    theId <- maybe newFormIdent return theId'
    let res = FormSuccess $ lookup name fenv
    let fi = FieldInfo
            { fiLabel = string label
            , fiTooltip = tooltip
            , fiIdent = theId
            , fiInput = fileWidget theId name False
            , fiErrors = Nothing
            , fiRequired = True
            }
    return (res, fi, Multipart)

fileWidget :: String -> String -> Bool -> GWidget s m ()
fileWidget theId name isReq = [$hamlet|
%input#$theId$!type=file!name=$name$!:isReq:required
|]
