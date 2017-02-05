{-# LANGUAGE OverloadedStrings #-}
module Yesod.Form.I18n.French (frenchFormMessage) where

import Yesod.Form.Types (FormMessage (..))
import Data.Monoid (mappend)
import Data.Text (Text)

frenchFormMessage :: FormMessage -> Text
frenchFormMessage (MsgInvalidInteger t) = "Entier invalide : " `Data.Monoid.mappend` t
frenchFormMessage (MsgInvalidNumber t) = "Nombre invalide : " `mappend` t
frenchFormMessage (MsgInvalidEntry t) = "Entrée invalide : " `mappend` t
frenchFormMessage MsgInvalidTimeFormat = "Heure invalide (elle doit être au format HH:MM ou HH:MM:SS"
frenchFormMessage MsgInvalidDay = "Date invalide (elle doit être au format AAAA-MM-JJ"
frenchFormMessage (MsgInvalidUrl t) = "Adresse Internet invalide : " `mappend` t
frenchFormMessage (MsgInvalidEmail t) = "Adresse électronique invalide : " `mappend` t
frenchFormMessage (MsgInvalidHour t) = "Heure invalide : " `mappend` t
frenchFormMessage (MsgInvalidMinute t) = "Minutes invalides : " `mappend` t
frenchFormMessage (MsgInvalidSecond t) = "Secondes invalides  " `mappend` t
frenchFormMessage MsgCsrfWarning = "Afin d'empêcher les attaques CSRF, veuillez ré-envoyer ce formulaire"
frenchFormMessage MsgValueRequired = "Ce champ est requis"
frenchFormMessage (MsgInputNotFound t) = "Entrée non trouvée : " `mappend` t
frenchFormMessage MsgSelectNone = "<Rien>"
frenchFormMessage (MsgInvalidBool t) = "Booléen invalide : " `mappend` t
frenchFormMessage MsgBoolYes = "Oui"
frenchFormMessage MsgBoolNo = "Non"
frenchFormMessage MsgDelete = "Détruire ?"
