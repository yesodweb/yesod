{-# LANGUAGE OverloadedStrings #-}
module Yesod.Form.I18n.Swedish where

import Yesod.Form.Types (FormMessage (..))
import Data.Monoid (mappend)
import Data.Text (Text)

swedishFormMessage :: FormMessage -> Text
swedishFormMessage (MsgInvalidInteger t) = "Ogiltigt antal: " `Data.Monoid.mappend` t
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
