{-# LANGUAGE QuasiQuotes #-}
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
import Yesod.Widget
import Data.Time (Day, TimeOfDay)
import Text.Hamlet
import Data.Monoid
import Control.Monad (join)
import Data.Maybe (fromMaybe)

stringField :: FormFieldSettings -> FormletField sub y String
stringField = requiredFieldHelper stringFieldProfile

maybeStringField :: FormFieldSettings -> FormletField sub y (Maybe String)
maybeStringField = optionalFieldHelper stringFieldProfile

intInput :: Integral i => String -> FormInput sub master i
intInput n =
    mapFormXml fieldsToInput $
    requiredFieldHelper intFieldProfile (nameSettings n) Nothing

maybeIntInput :: Integral i => String -> FormInput sub master (Maybe i)
maybeIntInput n =
    mapFormXml fieldsToInput $
    optionalFieldHelper intFieldProfile (nameSettings n) Nothing

intField :: Integral i => FormFieldSettings -> FormletField sub y i
intField = requiredFieldHelper intFieldProfile

maybeIntField :: Integral i => FormFieldSettings -> FormletField sub y (Maybe i)
maybeIntField = optionalFieldHelper intFieldProfile

doubleField :: FormFieldSettings -> FormletField sub y Double
doubleField = requiredFieldHelper doubleFieldProfile

maybeDoubleField :: FormFieldSettings -> FormletField sub y (Maybe Double)
maybeDoubleField = optionalFieldHelper doubleFieldProfile

dayField :: FormFieldSettings -> FormletField sub y Day
dayField = requiredFieldHelper dayFieldProfile

maybeDayField :: FormFieldSettings -> FormletField sub y (Maybe Day)
maybeDayField = optionalFieldHelper dayFieldProfile

timeField :: FormFieldSettings -> FormletField sub y TimeOfDay
timeField = requiredFieldHelper timeFieldProfile

maybeTimeField :: FormFieldSettings -> FormletField sub y (Maybe TimeOfDay)
maybeTimeField = optionalFieldHelper timeFieldProfile

boolField :: FormFieldSettings -> Maybe Bool -> FormField sub y Bool
boolField ffs orig = GForm $ do
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
            { fiLabel = label
            , fiTooltip = tooltip
            , fiIdent = theId
            , fiInput = addBody [$hamlet|
%input#$theId$!type=checkbox!name=$name$!:val:checked
|]
            , fiErrors = case res of
                            FormFailure [x] -> Just $ string x
                            _ -> Nothing
            , fiRequired = True
            }
    return (res, [fi], UrlEncoded)

htmlField :: FormFieldSettings -> FormletField sub y Html
htmlField = requiredFieldHelper htmlFieldProfile

maybeHtmlField :: FormFieldSettings -> FormletField sub y (Maybe Html)
maybeHtmlField = optionalFieldHelper htmlFieldProfile

selectField :: Eq x => [(x, String)]
            -> FormFieldSettings
            -> Maybe x -> FormField sub master x
selectField pairs ffs initial = GForm $ do
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
            { fiLabel = label
            , fiTooltip = tooltip
            , fiIdent = theId
            , fiInput = addBody input
            , fiErrors = case res of
                            FormFailure [x] -> Just $ string x
                            _ -> Nothing
            , fiRequired = True
            }
    return (res, [fi], UrlEncoded)

maybeSelectField :: Eq x => [(x, String)]
                 -> FormFieldSettings
                 -> FormletField sub master (Maybe x)
maybeSelectField pairs ffs initial' = GForm $ do
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
            { fiLabel = label
            , fiTooltip = tooltip
            , fiIdent = theId
            , fiInput = addBody input
            , fiErrors = case res of
                            FormFailure [x] -> Just $ string x
                            _ -> Nothing
            , fiRequired = False
            }
    return (res, [fi], UrlEncoded)

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
    let xml = addBody [$hamlet|
%input#$n$!type=checkbox!name=$n$
|]
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

urlField :: FormFieldSettings -> FormletField sub y String
urlField = requiredFieldHelper urlFieldProfile

maybeUrlField :: FormFieldSettings -> FormletField sub y (Maybe String)
maybeUrlField = optionalFieldHelper urlFieldProfile

urlInput :: String -> FormInput sub master String
urlInput n =
    mapFormXml fieldsToInput $
    requiredFieldHelper urlFieldProfile (nameSettings n) Nothing

emailField :: FormFieldSettings -> FormletField sub y String
emailField = requiredFieldHelper emailFieldProfile

maybeEmailField :: FormFieldSettings -> FormletField sub y (Maybe String)
maybeEmailField = optionalFieldHelper emailFieldProfile

emailInput :: String -> FormInput sub master String
emailInput n =
    mapFormXml fieldsToInput $
    requiredFieldHelper emailFieldProfile (nameSettings n) Nothing

textareaField :: FormFieldSettings -> FormletField sub y Textarea
textareaField = requiredFieldHelper textareaFieldProfile

maybeTextareaField :: FormFieldSettings -> FormletField sub y (Maybe Textarea)
maybeTextareaField = optionalFieldHelper textareaFieldProfile

hiddenField :: FormFieldSettings -> FormletField sub y String
hiddenField = requiredFieldHelper hiddenFieldProfile

maybeHiddenField :: FormFieldSettings -> FormletField sub y (Maybe String)
maybeHiddenField = optionalFieldHelper hiddenFieldProfile
