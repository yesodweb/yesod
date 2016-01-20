{-# LANGUAGE OverloadedStrings #-}
module Yesod.Form.I18n.Croatian where

import Yesod.Form.Types (FormMessage (..))
import Data.Monoid (mappend)
import Data.Text (Text)

englishFormMessage :: FormMessage -> Text
englishFormMessage (MsgInvalidInteger t) = "Cjelobrojna vrijednost nije valjana: " `mappend` t
englishFormMessage (MsgInvalidNumber t)  = "Broj nije valjan: " `mappend` t
englishFormMessage (MsgInvalidEntry t)   = "Unos nije valjan: " `mappend` t
englishFormMessage MsgInvalidTimeFormat  = "Vrijeme nije valjano, mora biti u obliku HH:MM[:SS]"
englishFormMessage MsgInvalidDay         = "Dan nije valjan, mora biti u obliku GGGG-MM-DD"
englishFormMessage (MsgInvalidUrl t)     = "URL adresa nije valjana: " `mappend` t
englishFormMessage (MsgInvalidEmail t)   = "Adresa e-pošte nije valjana: " `mappend` t
englishFormMessage (MsgInvalidHour t)    = "Sat nije valjan: " `mappend` t
englishFormMessage (MsgInvalidMinute t)  = "Minuta nije valjana: " `mappend` t
englishFormMessage (MsgInvalidSecond t)  = "Sekunda nije valjana: " `mappend` t
englishFormMessage MsgCsrfWarning        = "Potvrdite slanje obrasca radi zaštite od XSRF napada,"
englishFormMessage MsgValueRequired      = "Potrebno je unijeti vrijednost"
englishFormMessage (MsgInputNotFound t)  = "Unos nije pronađen: " `mappend` t
englishFormMessage MsgSelectNone         = "<nema>"
englishFormMessage (MsgInvalidBool t)    = "Logička vrijednost nije valjana: " `mappend` t
englishFormMessage MsgBoolYes            = "Da"
englishFormMessage MsgBoolNo             = "Ne"
englishFormMessage MsgDelete             = "Izbrisati?"
