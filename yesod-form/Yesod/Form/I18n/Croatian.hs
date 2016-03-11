{-# LANGUAGE OverloadedStrings #-}
module Yesod.Form.I18n.Croatian where

import Yesod.Form.Types (FormMessage (..))
import Data.Monoid (mappend)
import Data.Text (Text)

croatianFormMessage :: FormMessage -> Text
croatianFormMessage (MsgInvalidInteger t) = "Cjelobrojna vrijednost nije valjana: " `mappend` t
croatianFormMessage (MsgInvalidNumber t)  = "Broj nije valjan: " `mappend` t
croatianFormMessage (MsgInvalidEntry t)   = "Unos nije valjan: " `mappend` t
croatianFormMessage MsgInvalidTimeFormat  = "Vrijeme nije valjano, mora biti u obliku HH:MM[:SS]"
croatianFormMessage MsgInvalidDay         = "Dan nije valjan, mora biti u obliku GGGG-MM-DD"
croatianFormMessage (MsgInvalidUrl t)     = "URL adresa nije valjana: " `mappend` t
croatianFormMessage (MsgInvalidEmail t)   = "Adresa e-pošte nije valjana: " `mappend` t
croatianFormMessage (MsgInvalidHour t)    = "Sat nije valjan: " `mappend` t
croatianFormMessage (MsgInvalidMinute t)  = "Minuta nije valjana: " `mappend` t
croatianFormMessage (MsgInvalidSecond t)  = "Sekunda nije valjana: " `mappend` t
croatianFormMessage MsgCsrfWarning        = "Potvrdite slanje obrasca radi zaštite od XSRF napada,"
croatianFormMessage MsgValueRequired      = "Potrebno je unijeti vrijednost"
croatianFormMessage (MsgInputNotFound t)  = "Unos nije pronađen: " `mappend` t
croatianFormMessage MsgSelectNone         = "<nema>"
croatianFormMessage (MsgInvalidBool t)    = "Logička vrijednost nije valjana: " `mappend` t
croatianFormMessage MsgBoolYes            = "Da"
croatianFormMessage MsgBoolNo             = "Ne"
croatianFormMessage MsgDelete             = "Izbrisati?"
