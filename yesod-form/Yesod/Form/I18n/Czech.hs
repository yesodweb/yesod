{-# LANGUAGE OverloadedStrings #-}
module Yesod.Form.I18n.Czech where

import Yesod.Form.Types (FormMessage (..))
import Data.Monoid (mappend)
import Data.Text (Text)

englishFormMessage :: FormMessage -> Text
englishFormMessage (MsgInvalidInteger t) = "Neplatné celé číslo: " `mappend` t
englishFormMessage (MsgInvalidNumber t) = "Neplatné číslo: " `mappend` t
englishFormMessage (MsgInvalidEntry t) = "Neplatná položka: " `mappend` t
englishFormMessage MsgInvalidTimeFormat = "Neplatný čas, musí být ve formátu HH:MM[:SS]"
englishFormMessage MsgInvalidDay = "Neplatný den, musí být formátu YYYY-MM-DD"
englishFormMessage (MsgInvalidUrl t) = "Neplatná URL: " `mappend` t
englishFormMessage (MsgInvalidEmail t) = "Neplatná e-mailová adresa: " `mappend` t
englishFormMessage (MsgInvalidHour t) = "Neplatná hodina: " `mappend` t
englishFormMessage (MsgInvalidMinute t) = "Neplatná minuta: " `mappend` t
englishFormMessage (MsgInvalidSecond t) = "Neplatná sekunda: " `mappend` t
englishFormMessage MsgCsrfWarning = "Prosím potvrďte odeslání formuláře jako ochranu před útekem „cross-site request forgery“."
englishFormMessage MsgValueRequired = "Hodnota je vyžadována"
englishFormMessage (MsgInputNotFound t) = "Vstup nebyl nalezen: " `mappend` t
englishFormMessage MsgSelectNone = "<Nic>"
englishFormMessage (MsgInvalidBool t) = "Neplatná pravdivostní hodnota: " `mappend` t
englishFormMessage MsgBoolYes = "Ano"
englishFormMessage MsgBoolNo = "Ne"
englishFormMessage MsgDelete = "Smazat?"
