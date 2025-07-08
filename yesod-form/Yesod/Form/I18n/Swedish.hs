{-# LANGUAGE OverloadedStrings #-}

module Yesod.Form.I18n.Swedish where

import Yesod.Form.Types (FormMessage (..))
import Data.Text (Text)

swedishFormMessage :: FormMessage -> Text
swedishFormMessage (MsgInvalidInteger t) = "Ogiltigt antal: " `mappend` t
swedishFormMessage (MsgInvalidNumber t) = "Ogiltigt nummer: " `mappend` t
swedishFormMessage (MsgInvalidEntry t) = "Invalid entry: " `mappend` t
swedishFormMessage MsgInvalidTimeFormat = "Ogiltigt klockslag, måste vara på formatet HH:MM[:SS]"
swedishFormMessage MsgInvalidDay = "Ogiltigt datum, måste vara på formatet ÅÅÅÅ-MM-DD"
swedishFormMessage (MsgInvalidUrl t) = "Ogiltig URL: " `mappend` t
swedishFormMessage (MsgInvalidEmail t) = "Ogiltig epostadress: " `mappend` t
swedishFormMessage (MsgInvalidHour t) = "Ogiltig timme: " `mappend` t
swedishFormMessage (MsgInvalidMinute t) = "Ogiltig minut: " `mappend` t
swedishFormMessage (MsgInvalidSecond t) = "Ogiltig sekund: " `mappend` t
swedishFormMessage MsgValueRequired = "Fältet är obligatoriskt"
swedishFormMessage (MsgInputNotFound t) = "Fältet hittades ej: " `mappend` t
swedishFormMessage MsgSelectNone = "<Ingenting>"
swedishFormMessage (MsgInvalidBool t) = "Ogiltig boolesk: " `mappend` t
swedishFormMessage MsgBoolYes = "Ja"
swedishFormMessage MsgBoolNo = "Nej"
swedishFormMessage MsgDelete = "Radera?"
swedishFormMessage MsgCsrfWarning = "Som skydd mot \"cross-site request forgery\" attacker, vänligen bekräfta skickandet av formuläret."
swedishFormMessage (MsgInvalidHexColorFormat t) = "Ogiltig färg, måste vara i #rrggbb hexadecimalt format: " `mappend` t
swedishFormMessage (MsgInvalidDatetimeFormat t) = "Ogiltig datumtid, måste vara i formatet ÅÅÅÅ-MM-DD(T| )TT:MM[:SS]: " `mappend` t
