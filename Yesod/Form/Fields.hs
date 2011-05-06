{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-} -- FIXME remove
module Yesod.Form.Fields
    ( -- * Fields
      -- ** Required
      stringField
    , passwordField
    , textareaField
    , hiddenField
    , intField
    , doubleField
    , dayField
    , timeField
    , htmlField
    , selectField
    , radioField
    , boolField
    , emailField
    , searchField
    , urlField
    , fileField
      -- ** Optional
    , maybeStringField
    , maybePasswordField
    , maybeTextareaField
    , maybeHiddenField
    , maybeIntField
    , maybeDoubleField
    , maybeDayField
    , maybeTimeField
    , maybeHtmlField
    , maybeSelectField
    , maybeRadioField
    , maybeEmailField
    , maybeSearchField
    , maybeUrlField
    , maybeFileField
    {- FIXME
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
    -}
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
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text, unpack)
import qualified Data.Text as T

#if __GLASGOW_HASKELL__ >= 700
#define HAMLET hamlet
#else
#define HAMLET $hamlet
#endif

stringField = requiredFieldHelper stringFieldProfile

maybeStringField = optionalFieldHelper stringFieldProfile

passwordField = requiredFieldHelper passwordFieldProfile

maybePasswordField = optionalFieldHelper passwordFieldProfile

{- FIXME
intInput n =
    mapFormXml fieldsToInput $
    requiredFieldHelper intFieldProfile (nameSettings n) Nothing

maybeIntInput n =
    mapFormXml fieldsToInput $
    optionalFieldHelper intFieldProfile (nameSettings n) Nothing
-}

intField = requiredFieldHelper intFieldProfile

maybeIntField = optionalFieldHelper intFieldProfile

doubleField = requiredFieldHelper doubleFieldProfile

maybeDoubleField = optionalFieldHelper doubleFieldProfile

dayField = requiredFieldHelper dayFieldProfile

maybeDayField = optionalFieldHelper dayFieldProfile

timeField = requiredFieldHelper timeFieldProfile

maybeTimeField = optionalFieldHelper timeFieldProfile

boolField ffs orig = do
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
            { fiLabel = toHtml label
            , fiTooltip = tooltip
            , fiIdent = theId
            , fiInput = [HAMLET|
<input id="#{theId}" type="checkbox" name="#{name}" :val:checked="">
|]
            , fiErrors = case res of
                            FormFailure [x] -> Just $ toHtml x
                            _ -> Nothing
            , fiRequired = True
            }
    return (res, fi, UrlEncoded)

htmlField = requiredFieldHelper htmlFieldProfile

maybeHtmlField = optionalFieldHelper htmlFieldProfile

selectField pairs ffs initial = do
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
                    case reads $ unpack x of
                        (x', _):_ ->
                            case lookup x' pairs' of
                                Nothing -> FormFailure ["Invalid entry"]
                                Just (y, _) -> FormSuccess y
                        [] -> FormFailure ["Invalid entry"]
    let isSelected x =
            case res of
                FormSuccess y -> x == y
                _ -> Just x == initial
    let input =
#if __GLASGOW_HASKELL__ >= 700
                [hamlet|
#else
                [$hamlet|
#endif
<select id="#{theId}" name="#{name}">
    <option value="none">
    $forall pair <- pairs'
        <option value="#{show (fst pair)}" :isSelected (fst (snd pair)):selected="">#{snd (snd pair)}
|]
    let fi = FieldInfo
            { fiLabel = toHtml label
            , fiTooltip = tooltip
            , fiIdent = theId
            , fiInput = input
            , fiErrors = case res of
                            FormFailure [x] -> Just $ toHtml x
                            _ -> Nothing
            , fiRequired = True
            }
    return (res, fi, UrlEncoded)

maybeSelectField pairs ffs initial' = do
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
                    case reads $ unpack x of
                        (x', _):_ ->
                            case lookup x' pairs' of
                                Nothing -> FormFailure ["Invalid entry"]
                                Just (y, _) -> FormSuccess $ Just y
                        [] -> FormFailure ["Invalid entry"]
    let isSelected x =
            case res of
                FormSuccess y -> Just x == y
                _ -> Just x == initial
    let input =
#if __GLASGOW_HASKELL__ >= 700
                [hamlet|
#else
                [$hamlet|
#endif
<select id="#{theId}" name="#{name}">
    <option value="none">
    $forall pair <- pairs'
        <option value="#{show (fst pair)}" :isSelected (fst (snd pair)):selected="">#{snd (snd pair)}
|]
    let fi = FieldInfo
            { fiLabel = toHtml label
            , fiTooltip = tooltip
            , fiIdent = theId
            , fiInput = input
            , fiErrors = case res of
                            FormFailure [x] -> Just $ toHtml x
                            _ -> Nothing
            , fiRequired = False
            }
    return (res, fi, UrlEncoded)

{- FIXME
stringInput :: Text -> FormInput sub master Text
stringInput n =
    mapFormXml fieldsToInput $
    requiredFieldHelper stringFieldProfile (nameSettings n) Nothing

maybeStringInput :: Text -> FormInput sub master (Maybe Text)
maybeStringInput n =
    mapFormXml fieldsToInput $
    optionalFieldHelper stringFieldProfile (nameSettings n) Nothing

boolInput :: Text -> FormInput sub master Bool
boolInput n = GForm $ do
    env <- askParams
    let res = case lookup n env of
                Nothing -> FormSuccess False
                Just "" -> FormSuccess False
                Just "false" -> FormSuccess False
                Just _ -> FormSuccess True
    let xml = [HAMLET|
    <input id="#{n}" type="checkbox" name="#{n}">
|]
    return (res, [xml], UrlEncoded)

dayInput :: Text -> FormInput sub master Day
dayInput n =
    mapFormXml fieldsToInput $
    requiredFieldHelper dayFieldProfile (nameSettings n) Nothing

maybeDayInput :: Text -> FormInput sub master (Maybe Day)
maybeDayInput n =
    mapFormXml fieldsToInput $
    optionalFieldHelper dayFieldProfile (nameSettings n) Nothing
-}

nameSettings :: Text -> FormFieldSettings
nameSettings n = FormFieldSettings mempty mempty (Just n) (Just n)

urlField = requiredFieldHelper urlFieldProfile

maybeUrlField = optionalFieldHelper urlFieldProfile

{- FIXME
urlInput :: Text -> FormInput sub master Text
urlInput n =
    mapFormXml fieldsToInput $
    requiredFieldHelper urlFieldProfile (nameSettings n) Nothing
-}

emailField = requiredFieldHelper emailFieldProfile

maybeEmailField = optionalFieldHelper emailFieldProfile

{- FIXME
emailInput :: Text -> FormInput sub master Text
emailInput n =
    mapFormXml fieldsToInput $
    requiredFieldHelper emailFieldProfile (nameSettings n) Nothing
-}

searchField = requiredFieldHelper . searchFieldProfile

maybeSearchField = optionalFieldHelper . searchFieldProfile

textareaField = requiredFieldHelper textareaFieldProfile

maybeTextareaField = optionalFieldHelper textareaFieldProfile

hiddenField = requiredFieldHelper hiddenFieldProfile

maybeHiddenField = optionalFieldHelper hiddenFieldProfile

fileField ffs = do
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
            { fiLabel = toHtml label
            , fiTooltip = tooltip
            , fiIdent = theId
            , fiInput = fileWidget theId name True
            , fiErrors = case res of
                            FormFailure [x] -> Just $ toHtml x
                            _ -> Nothing
            , fiRequired = True
            }
    let res' = case res of
                FormFailure [e] -> FormFailure [T.concat [label, ": ", e]]
                _ -> res
    return (res', fi, Multipart)

maybeFileField ffs = do
    fenv <- lift $ lift ask
    let (FormFieldSettings label tooltip theId' name') = ffs
    name <- maybe newFormIdent return name'
    theId <- maybe newFormIdent return theId'
    let res = FormSuccess $ lookup name fenv
    let fi = FieldInfo
            { fiLabel = toHtml label
            , fiTooltip = tooltip
            , fiIdent = theId
            , fiInput = fileWidget theId name False
            , fiErrors = Nothing
            , fiRequired = True
            }
    return (res, fi, Multipart)

fileWidget :: Text -> Text -> Bool -> GWidget s m ()
fileWidget theId name isReq = [HAMLET|
<input id="#{theId}" type="file" name="#{name}" :isReq:required="">
|]

radioField pairs ffs initial = do
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
                    case reads $ unpack x of
                        (x', _):_ ->
                            case lookup x' pairs' of
                                Nothing -> FormFailure ["Invalid entry"]
                                Just (y, _) -> FormSuccess y
                        [] -> FormFailure ["Invalid entry"]
    let isSelected x =
            case res of
                FormSuccess y -> x == y
                _ -> Just x == initial
    let input = [HAMLET|
<div id="#{theId}">
   $forall pair <- pairs'
       <div>
           <input id="#{theId}-#{show (fst pair)}" type="radio" name="#{name}" value="#{show (fst pair)}" :isSelected (fst (snd pair)):checked="">
           <label for="#{name}-#{show (fst pair)}">#{snd (snd pair)}
|]
    let fi = FieldInfo
           { fiLabel = toHtml label
           , fiTooltip = tooltip
           , fiIdent = theId
           , fiInput = input
           , fiErrors = case res of
                           FormFailure [x] -> Just $ toHtml x
                           _ -> Nothing
           , fiRequired = True
           }
    return (res, fi, UrlEncoded)

maybeRadioField pairs ffs initial' = do
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
                    case reads $ unpack x of
                        (x', _):_ ->
                            case lookup x' pairs' of
                                Nothing -> FormFailure ["Invalid entry"]
                                Just (y, _) -> FormSuccess $ Just y
                        [] -> FormFailure ["Invalid entry"]
    let isSelected x =
            case res of
                FormSuccess y -> Just x == y
                _ -> Just x == initial
    let isNone =
            case res of
                FormSuccess Nothing -> True
                FormSuccess Just{} -> False
                _ -> isNothing initial
    let input =
#if __GLASGOW_HASKELL__ >= 700
                [hamlet|
#else
                [$hamlet|
#endif
<div id="#{theId}">
   $forall pair <- pairs'
       <div>
           <input id="#{theId}-none" type="radio" name="#{name}" value="none" :isNone:checked="">None
       <div>
           <input id="#{theId}-#{show (fst pair)}" type="radio" name="#{name}" value="#{show (fst pair)}" :isSelected (fst (snd pair)):checked="">
           <label for="#{name}-#{show (fst pair)}">#{snd (snd pair)}
|]
    let fi = FieldInfo
           { fiLabel = toHtml label
           , fiTooltip = tooltip
           , fiIdent = theId
           , fiInput = input
           , fiErrors = case res of
                           FormFailure [x] -> Just $ toHtml x
                           _ -> Nothing
           , fiRequired = False
           }
    return (res, fi, UrlEncoded)
