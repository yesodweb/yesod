{-# LANGUAGE OverloadedStrings #-}

module Yesod.Form.I18n.Spanish where

import Yesod.Form.Types (FormMessage (..))
import Data.Monoid (mappend)
import Data.Text (Text)

spanishFormMessage :: FormMessage -> Text
spanishFormMessage (MsgInvalidInteger t) = "Número entero inválido: " `Data.Monoid.mappend` t
spanishFormMessage (MsgInvalidNumber t) = "Número inválido: " `mappend` t
spanishFormMessage (MsgInvalidEntry t) = "Entrada inválida: " `mappend` t
spanishFormMessage MsgInvalidTimeFormat = "Hora inválida, debe tener el formato HH:MM[:SS]"
spanishFormMessage MsgInvalidDay = "Fecha inválida, debe tener el formato AAAA-MM-DD"
spanishFormMessage (MsgInvalidUrl t) = "URL inválida: " `mappend` t
spanishFormMessage (MsgInvalidEmail t) = "Dirección de correo electrónico inválida: " `mappend` t
spanishFormMessage (MsgInvalidHour t) = "Hora inválida: " `mappend` t
spanishFormMessage (MsgInvalidMinute t) = "Minuto inválido: " `mappend` t
spanishFormMessage (MsgInvalidSecond t) = "Segundo inválido: " `mappend` t
spanishFormMessage MsgCsrfWarning = "Como protección contra ataques CSRF, confirme su envío por favor."
spanishFormMessage MsgValueRequired = "Se requiere un valor"
spanishFormMessage (MsgInputNotFound t) = "Entrada no encontrada: " `mappend` t
spanishFormMessage MsgSelectNone = "<Ninguno>"
spanishFormMessage (MsgInvalidBool t) = "Booleano inválido: " `mappend` t
spanishFormMessage MsgBoolYes = "Sí"
spanishFormMessage MsgBoolNo = "No"
spanishFormMessage MsgDelete = "¿Eliminar?"
