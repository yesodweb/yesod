{-# LANGUAGE OverloadedStrings #-}

module Yesod.Form.I18n.Romanian where

import Yesod.Form.Types (FormMessage (..))
import Data.Text (Text)

-- | Romanian translation
--
-- @since 1.7.5
romanianFormMessage :: FormMessage -> Text
romanianFormMessage (MsgInvalidInteger t) = "Număr întreg nevalid: " `mappend` t
romanianFormMessage (MsgInvalidNumber t) = "Număr nevalid: " `mappend` t
romanianFormMessage (MsgInvalidEntry t) = "Valoare nevalidă: " `mappend` t
romanianFormMessage MsgInvalidTimeFormat = "Oră nevalidă. Formatul necesar este HH:MM[:SS]"
romanianFormMessage MsgInvalidDay = "Dată nevalidă. Formatul necesar este AAAA-LL-ZZ"
romanianFormMessage (MsgInvalidUrl t) = "Adresă URL nevalidă: " `mappend` t
romanianFormMessage (MsgInvalidEmail t) = "Adresă de e-mail nevalidă: " `mappend` t
romanianFormMessage (MsgInvalidHour t) = "Oră nevalidă: " `mappend` t
romanianFormMessage (MsgInvalidMinute t) = "Minut nevalid: " `mappend` t
romanianFormMessage (MsgInvalidSecond t) = "Secundă nevalidă: " `mappend` t
romanianFormMessage MsgCsrfWarning = "Ca protecție împotriva atacurilor CSRF, vă rugăm să confirmați trimiterea formularului."
romanianFormMessage MsgValueRequired = "Câmp obligatoriu"
romanianFormMessage (MsgInputNotFound t) = "Valoare inexistentă: " `mappend` t
romanianFormMessage MsgSelectNone = "<Niciuna>"
romanianFormMessage (MsgInvalidBool t) = "Valoare booleană nevalidă: " `mappend` t
romanianFormMessage MsgBoolYes = "Da"
romanianFormMessage MsgBoolNo = "Nu"
romanianFormMessage MsgDelete = "Șterge?"
romanianFormMessage (MsgInvalidHexColorFormat t) = "Culoare nevalidă. Formatul necesar este #rrggbb în hexazecimal: " `mappend` t
romanianFormMessage (MsgInvalidDatetimeFormat t) = "Data și ora nevalidă, trebuie să fie în format AAAA-LL-ZZ(T| )HH:MM[:SS]: " `mappend` t
