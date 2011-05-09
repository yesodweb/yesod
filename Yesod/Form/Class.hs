{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Yesod.Form.Class
    ( ToForm (..)
    , ToField (..)
    ) where

import Text.Hamlet
import Yesod.Widget (GGWidget)
import Yesod.Form.Fields
import Yesod.Form.Types
import Yesod.Form.Functions (areq, aopt)
import Data.Int (Int64)
import Data.Time (Day, TimeOfDay)
import Data.Text (Text)
import Yesod.Handler (GGHandler)

class ToForm a master monad where
    toForm :: AForm ([FieldView (GGWidget master monad ())] -> [FieldView (GGWidget master monad ())]) monad a

class ToField a master monad where
    toField :: FieldSettings -> Maybe a -> AForm ([FieldView (GGWidget master monad ())] -> [FieldView (GGWidget master monad ())]) monad a

{- FIXME
instance ToFormField String y where
    toFormField = stringField
instance ToFormField (Maybe String) y where
    toFormField = maybeStringField
-}

instance Monad m => ToField Text master (GGHandler sub master m) where
    toField = areq textField
instance Monad m => ToField (Maybe Text) master (GGHandler sub master m) where
    toField = aopt textField

instance Monad m => ToField Int master (GGHandler sub master m) where
    toField = areq intField
instance Monad m => ToField (Maybe Int) master (GGHandler sub master m) where
    toField = aopt intField

instance Monad m => ToField Int64 master (GGHandler sub master m) where
    toField = areq intField
instance Monad m => ToField (Maybe Int64) master (GGHandler sub master m) where
    toField = aopt intField

instance Monad m => ToField Double master (GGHandler sub master m) where
    toField = areq doubleField
instance Monad m => ToField (Maybe Double) master (GGHandler sub master m) where
    toField = aopt doubleField

instance Monad m => ToField Day master (GGHandler sub master m) where
    toField = areq dayField
instance Monad m => ToField (Maybe Day) master (GGHandler sub master m) where
    toField = aopt dayField

instance Monad m => ToField TimeOfDay master (GGHandler sub master m) where
    toField = areq timeField
instance Monad m => ToField (Maybe TimeOfDay) master (GGHandler sub master m) where
    toField = aopt timeField

instance Monad m => ToField Html master (GGHandler sub master m) where
    toField = areq htmlField
instance Monad m => ToField (Maybe Html) master (GGHandler sub master m) where
    toField = aopt htmlField

instance Monad m => ToField Textarea master (GGHandler sub master m) where
    toField = areq textareaField
instance Monad m => ToField (Maybe Textarea) master (GGHandler sub master m) where
    toField = aopt textareaField

{- FIXME
instance ToFormField Bool y where
    toFormField = boolField
-}
