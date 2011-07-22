{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Yesod.Message (RenderMessage)

class ToForm a master monad where
    toForm :: AForm ([FieldView (GGWidget master monad ())] -> [FieldView (GGWidget master monad ())]) master monad a

class ToField a master monad where
    toField :: RenderMessage master msg => FieldSettings msg -> Maybe a -> AForm ([FieldView (GGWidget master monad ())] -> [FieldView (GGWidget master monad ())]) master monad a

{- FIXME
instance ToFormField String y where
    toFormField = stringField
instance ToFormField (Maybe String) y where
    toFormField = maybeStringField
-}

instance (Monad m, RenderMessage master FormMessage) => ToField Text master (GGHandler sub master m) where
    toField = areq textField
instance (Monad m, RenderMessage master FormMessage) => ToField (Maybe Text) master (GGHandler sub master m) where
    toField = aopt textField

instance (Monad m, RenderMessage master FormMessage) => ToField Int master (GGHandler sub master m) where
    toField = areq intField
instance (Monad m, RenderMessage master FormMessage) => ToField (Maybe Int) master (GGHandler sub master m) where
    toField = aopt intField

instance (Monad m, RenderMessage master FormMessage) => ToField Int64 master (GGHandler sub master m) where
    toField = areq intField
instance (Monad m, RenderMessage master FormMessage) => ToField (Maybe Int64) master (GGHandler sub master m) where
    toField = aopt intField

instance (Monad m, RenderMessage master FormMessage) => ToField Double master (GGHandler sub master m) where
    toField = areq doubleField
instance (Monad m, RenderMessage master FormMessage) => ToField (Maybe Double) master (GGHandler sub master m) where
    toField = aopt doubleField

instance (Monad m, RenderMessage master FormMessage) => ToField Day master (GGHandler sub master m) where
    toField = areq dayField
instance (Monad m, RenderMessage master FormMessage) => ToField (Maybe Day) master (GGHandler sub master m) where
    toField = aopt dayField

instance (Monad m, RenderMessage master FormMessage) => ToField TimeOfDay master (GGHandler sub master m) where
    toField = areq timeField
instance (Monad m, RenderMessage master FormMessage) => ToField (Maybe TimeOfDay) master (GGHandler sub master m) where
    toField = aopt timeField

instance (Monad m, RenderMessage master FormMessage) => ToField Html master (GGHandler sub master m) where
    toField = areq htmlField
instance (Monad m, RenderMessage master FormMessage) => ToField (Maybe Html) master (GGHandler sub master m) where
    toField = aopt htmlField

instance (Monad m, RenderMessage master FormMessage) => ToField Textarea master (GGHandler sub master m) where
    toField = areq textareaField
instance (Monad m, RenderMessage master FormMessage) => ToField (Maybe Textarea) master (GGHandler sub master m) where
    toField = aopt textareaField

{- FIXME
instance ToFormField Bool y where
    toFormField = boolField
-}
