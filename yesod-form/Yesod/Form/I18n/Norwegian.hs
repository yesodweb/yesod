{-# LANGUAGE OverloadedStrings #-}
module Yesod.Form.I18n.Norwegian where

import Yesod.Form.Types (FormMessage (..))
import Data.Monoid (mappend)
import Data.Text (Text)

norwegianBokmålFormMessage :: FormMessage -> Text
norwegianBokmålFormMessage (MsgInvalidInteger t) = "Ugyldig antall: " `mappend` t
norwegianBokmålFormMessage (MsgInvalidNumber t) = "Ugyldig nummer: " `mappend` t
norwegianBokmålFormMessage (MsgInvalidEntry t) = "Ugyldig oppføring: " `mappend` t
norwegianBokmålFormMessage MsgInvalidTimeFormat = "Ugyldig klokkeslett, må være i formatet HH:MM[:SS]"
norwegianBokmålFormMessage MsgInvalidDay = "Ugyldig dato, må være i formatet ÅÅÅÅ-MM-DD"
norwegianBokmålFormMessage (MsgInvalidUrl t) = "Ugyldig URL: " `mappend` t
norwegianBokmålFormMessage (MsgInvalidEmail t) = "Ugyldig e-postadresse: " `mappend` t
norwegianBokmålFormMessage (MsgInvalidHour t) = "Ugyldig time: " `mappend` t
norwegianBokmålFormMessage (MsgInvalidMinute t) = "Ugyldig minutt: " `mappend` t
norwegianBokmålFormMessage (MsgInvalidSecond t) = "Ugyldig sekund: " `mappend` t
norwegianBokmålFormMessage MsgValueRequired = "Feltet er obligatorisk"
norwegianBokmålFormMessage (MsgInputNotFound t) = "Feltet ble ikke funnet: " `mappend` t
norwegianBokmålFormMessage MsgSelectNone = "<Ingenting>"
norwegianBokmålFormMessage (MsgInvalidBool t) = "Ugyldig sannhetsverdi: " `mappend` t
norwegianBokmålFormMessage MsgBoolYes = "Ja"
norwegianBokmålFormMessage MsgBoolNo = "Nei"
norwegianBokmålFormMessage MsgDelete = "Slette?"
norwegianBokmålFormMessage MsgCsrfWarning = "Som beskyttelse mot «cross-site request forgery»-angrep, vennligst bekreft innsendt skjema."
