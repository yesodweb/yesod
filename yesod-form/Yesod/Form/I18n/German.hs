{-# LANGUAGE OverloadedStrings #-}
module Yesod.Form.I18n.German where

import Yesod.Form.Types (FormMessage (..))
import Data.Monoid (mappend)
import Data.Text (Text)

germanFormMessage :: FormMessage -> Text
germanFormMessage (MsgInvalidInteger t) = "Ungültige Ganzzahl: " `Data.Monoid.mappend` t
germanFormMessage (MsgInvalidNumber t) = "Ungültige Zahl: " `mappend` t
germanFormMessage (MsgInvalidEntry t) = "Ungültiger Eintrag: " `mappend` t
germanFormMessage MsgInvalidTimeFormat = "Ungültiges Zeitformat, HH:MM[:SS] Format erwartet"
germanFormMessage MsgInvalidDay = "Ungültiges Datum, JJJJ-MM-TT Format erwartet"
germanFormMessage (MsgInvalidUrl t) = "Ungültige URL: " `mappend` t
germanFormMessage (MsgInvalidEmail t) = "Ungültige e-Mail Adresse: " `mappend` t
germanFormMessage (MsgInvalidHour t) = "Ungültige Stunde: " `mappend` t
germanFormMessage (MsgInvalidMinute t) = "Ungültige Minute: " `mappend` t
germanFormMessage (MsgInvalidSecond t) = "Ungültige Sekunde: " `mappend` t
germanFormMessage MsgCsrfWarning = "Bitte bestätigen Sie ihre Eingabe, als Schutz gegen Cross-Site Forgery Angriffe"
germanFormMessage MsgValueRequired = "Wert wird benötigt"
germanFormMessage (MsgInputNotFound t) = "Eingabe nicht gefunden: " `mappend` t
germanFormMessage MsgSelectNone = "<Nichts>"
germanFormMessage (MsgInvalidBool t) = "Ungültiger Wahrheitswert: " `mappend` t
germanFormMessage MsgBoolYes = "Ja"
germanFormMessage MsgBoolNo = "Nein"
germanFormMessage MsgDelete = "Löschen?"
