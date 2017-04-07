{-# LANGUAGE OverloadedStrings #-}
module Yesod.Form.I18n.Dutch where

import Yesod.Form.Types (FormMessage (..))
import Data.Monoid (mappend)
import Data.Text (Text)

dutchFormMessage :: FormMessage -> Text
dutchFormMessage (MsgInvalidInteger t) = "Ongeldig aantal: " `Data.Monoid.mappend` t
dutchFormMessage (MsgInvalidNumber t)  = "Ongeldig getal: " `mappend` t
dutchFormMessage (MsgInvalidEntry t)   = "Ongeldige invoer: " `mappend` t
dutchFormMessage MsgInvalidTimeFormat  = "Ongeldige tijd, het juiste formaat is (UU:MM[:SS])"
dutchFormMessage MsgInvalidDay         = "Ongeldige datum, het juiste formaat is (JJJJ-MM-DD)"
dutchFormMessage (MsgInvalidUrl t)     = "Ongeldige URL: " `mappend` t
dutchFormMessage (MsgInvalidEmail t)   = "Ongeldig e-mail adres: " `mappend` t
dutchFormMessage (MsgInvalidHour t)    = "Ongeldig uur: " `mappend` t
dutchFormMessage (MsgInvalidMinute t)  = "Ongeldige minuut: " `mappend` t
dutchFormMessage (MsgInvalidSecond t)  = "Ongeldige seconde: " `mappend` t
dutchFormMessage MsgCsrfWarning        = "Bevestig het indienen van het formulier, dit als veiligheidsmaatregel tegen \"cross-site request forgery\" aanvallen."
dutchFormMessage MsgValueRequired      = "Verplicht veld"
dutchFormMessage (MsgInputNotFound t)  = "Geen invoer gevonden: " `mappend` t
dutchFormMessage MsgSelectNone         = "<Geen>"
dutchFormMessage (MsgInvalidBool t)    = "Ongeldige waarheidswaarde: " `mappend` t
dutchFormMessage MsgBoolYes            = "Ja"
dutchFormMessage MsgBoolNo             = "Nee"
dutchFormMessage MsgDelete             = "Verwijderen?"
