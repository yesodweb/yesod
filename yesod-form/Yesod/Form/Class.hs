{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Yesod.Form.Class
    ( {- FIXME ToForm (..)
    , -} ToField (..)
    ) where

import Text.Hamlet
import Yesod.Form.Fields
import Yesod.Form.Types
import Yesod.Form.Functions (areq, aopt)
import Data.Int (Int64)
import Data.Time (Day, TimeOfDay)
import Data.Text (Text)
import Yesod.Message (RenderMessage)

{-
class ToForm a where
    toForm :: AForm sub master a
-}

class ToField a master where
    toField :: (RenderMessage master msg, RenderMessage master FormMessage)
            => FieldSettings msg -> Maybe a -> AForm sub master a

{- FIXME
instance ToFormField String y where
    toFormField = stringField
instance ToFormField (Maybe String) y where
    toFormField = maybeStringField
-}

instance ToField Text master where
    toField = areq textField
instance ToField (Maybe Text) master where
    toField = aopt textField

instance ToField Int master where
    toField = areq intField
instance ToField (Maybe Int) master where
    toField = aopt intField

instance ToField Int64 master where
    toField = areq intField
instance ToField (Maybe Int64) master where
    toField = aopt intField

instance ToField Double master where
    toField = areq doubleField
instance ToField (Maybe Double) master where
    toField = aopt doubleField

instance ToField Day master where
    toField = areq dayField
instance ToField (Maybe Day) master where
    toField = aopt dayField

instance ToField TimeOfDay master where
    toField = areq timeField
instance ToField (Maybe TimeOfDay) master where
    toField = aopt timeField

instance ToField Html master where
    toField = areq htmlField
instance ToField (Maybe Html) master where
    toField = aopt htmlField

instance ToField Textarea master where
    toField = areq textareaField
instance ToField (Maybe Textarea) master where
    toField = aopt textareaField

{- FIXME
instance ToFormField Bool y where
    toFormField = boolField
-}
