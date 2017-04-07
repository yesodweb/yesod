{-# LANGUAGE OverloadedStrings #-}
module Yesod.Form.I18n.Czech where

import Yesod.Form.Types (FormMessage (..))
import Data.Monoid (mappend)
import Data.Text (Text)

czechFormMessage :: FormMessage -> Text
czechFormMessage (MsgInvalidInteger t) = "Neplatné celé číslo: " `Data.Monoid.mappend` t
czechFormMessage (MsgInvalidNumber t) = "Neplatné číslo: " `mappend` t
czechFormMessage (MsgInvalidEntry t) = "Neplatná položka: " `mappend` t
czechFormMessage MsgInvalidTimeFormat = "Neplatný čas, musí být ve formátu HH:MM[:SS]"
czechFormMessage MsgInvalidDay = "Neplatný den, musí být formátu YYYY-MM-DD"
czechFormMessage (MsgInvalidUrl t) = "Neplatná URL: " `mappend` t
czechFormMessage (MsgInvalidEmail t) = "Neplatná e-mailová adresa: " `mappend` t
czechFormMessage (MsgInvalidHour t) = "Neplatná hodina: " `mappend` t
czechFormMessage (MsgInvalidMinute t) = "Neplatná minuta: " `mappend` t
czechFormMessage (MsgInvalidSecond t) = "Neplatná sekunda: " `mappend` t
czechFormMessage MsgCsrfWarning = "Prosím potvrďte odeslání formuláře jako ochranu před útekem „cross-site request forgery“."
czechFormMessage MsgValueRequired = "Hodnota je vyžadována"
czechFormMessage (MsgInputNotFound t) = "Vstup nebyl nalezen: " `mappend` t
czechFormMessage MsgSelectNone = "<Nic>"
czechFormMessage (MsgInvalidBool t) = "Neplatná pravdivostní hodnota: " `mappend` t
czechFormMessage MsgBoolYes = "Ano"
czechFormMessage MsgBoolNo = "Ne"
czechFormMessage MsgDelete = "Smazat?"
