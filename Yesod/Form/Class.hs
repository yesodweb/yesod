{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Yesod.Form.Class
    ( ToForm (..)
    , ToFormField (..)
    ) where

import Text.Hamlet
import Yesod.Form.Fields
import Yesod.Form.Core
import Data.Int (Int64)
import Data.Time (Day, TimeOfDay)

class ToForm a y where
    toForm :: Maybe a -> Form sub y a
class ToFormField a y where
    toFormField :: FormFieldSettings -> Maybe a -> FormField sub y a

instance ToFormField String y where
    toFormField = stringField
instance ToFormField (Maybe String) y where
    toFormField = maybeStringField

instance ToFormField Int y where
    toFormField = intField
instance ToFormField (Maybe Int) y where
    toFormField = maybeIntField
instance ToFormField Int64 y where
    toFormField = intField
instance ToFormField (Maybe Int64) y where
    toFormField = maybeIntField

instance ToFormField Double y where
    toFormField = doubleField
instance ToFormField (Maybe Double) y where
    toFormField = maybeDoubleField

instance ToFormField Day y where
    toFormField = dayField
instance ToFormField (Maybe Day) y where
    toFormField = maybeDayField

instance ToFormField TimeOfDay y where
    toFormField = timeField
instance ToFormField (Maybe TimeOfDay) y where
    toFormField = maybeTimeField

instance ToFormField Bool y where
    toFormField = boolField

instance ToFormField Html y where
    toFormField = htmlField
instance ToFormField (Maybe Html) y where
    toFormField = maybeHtmlField
