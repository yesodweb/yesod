{-# LANGUAGE OverloadedStrings #-}
module Yesod.Form.I18n.Romanian where

import Yesod.Form.Types (FormMessage (..))
import Data.Monoid (mappend)
import Data.Text (Text)

-- |
-- Romanian translation
--
-- @since 1.7.5
englishFormMessage :: FormMessage -> Text
englishFormMessage (MsgInvalidInteger t) = "Număr întreg nevalid: " `Data.Monoid.mappend` t
englishFormMessage (MsgInvalidNumber t) = "Număr nevalid: " `mappend` t
englishFormMessage (MsgInvalidEntry t) = "Valoare nevalidă: " `mappend` t
englishFormMessage MsgInvalidTimeFormat = "Oră nevalidă. Formatul necesar este HH:MM[:SS]"
englishFormMessage MsgInvalidDay = "Dată nevalidă. Formatul necesar este AAAA-LL-ZZ"
englishFormMessage (MsgInvalidUrl t) = "Adresă URL nevalidă: " `mappend` t
englishFormMessage (MsgInvalidEmail t) = "Adresă de e-mail nevalidă: " `mappend` t
englishFormMessage (MsgInvalidHour t) = "Oră nevalidă: " `mappend` t
englishFormMessage (MsgInvalidMinute t) = "Minut nevalid: " `mappend` t
englishFormMessage (MsgInvalidSecond t) = "Secundă nevalidă: " `mappend` t
englishFormMessage MsgCsrfWarning = "Ca protecție împotriva atacurilor CSRF, vă rugăm să confirmați trimiterea formularului."
englishFormMessage MsgValueRequired = "Câmp obligatoriu"
englishFormMessage (MsgInputNotFound t) = "Valoare inexistentă: " `mappend` t
englishFormMessage MsgSelectNone = "<Niciuna>"
englishFormMessage (MsgInvalidBool t) = "Valoare booleană nevalidă: " `mappend` t
englishFormMessage MsgBoolYes = "Da"
englishFormMessage MsgBoolNo = "Nu"
englishFormMessage MsgDelete = "Șterge?"
englishFormMessage (MsgInvalidHexColorFormat t) = "Culoarea nevalidă. Formatul necesar este #rrggbb în hexazecimal: " `mappend` t
