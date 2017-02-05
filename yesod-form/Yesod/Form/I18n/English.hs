{-# LANGUAGE OverloadedStrings #-}
module Yesod.Form.I18n.English where

import Yesod.Form.Types (FormMessage (..))
import Data.Monoid (mappend)
import Data.Text (Text)

englishFormMessage :: FormMessage -> Text
englishFormMessage (MsgInvalidInteger t) = "Invalid integer: " `Data.Monoid.mappend` t
englishFormMessage (MsgInvalidNumber t) = "Invalid number: " `mappend` t
englishFormMessage (MsgInvalidEntry t) = "Invalid entry: " `mappend` t
englishFormMessage MsgInvalidTimeFormat = "Invalid time, must be in HH:MM[:SS] format"
englishFormMessage MsgInvalidDay = "Invalid day, must be in YYYY-MM-DD format"
englishFormMessage (MsgInvalidUrl t) = "Invalid URL: " `mappend` t
englishFormMessage (MsgInvalidEmail t) = "Invalid e-mail address: " `mappend` t
englishFormMessage (MsgInvalidHour t) = "Invalid hour: " `mappend` t
englishFormMessage (MsgInvalidMinute t) = "Invalid minute: " `mappend` t
englishFormMessage (MsgInvalidSecond t) = "Invalid second: " `mappend` t
englishFormMessage MsgCsrfWarning = "As a protection against cross-site request forgery attacks, please confirm your form submission."
englishFormMessage MsgValueRequired = "Value is required"
englishFormMessage (MsgInputNotFound t) = "Input not found: " `mappend` t
englishFormMessage MsgSelectNone = "<None>"
englishFormMessage (MsgInvalidBool t) = "Invalid boolean: " `mappend` t
englishFormMessage MsgBoolYes = "Yes"
englishFormMessage MsgBoolNo = "No"
englishFormMessage MsgDelete = "Delete?"
