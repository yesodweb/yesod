{-# LANGUAGE OverloadedStrings #-}
module Yesod.Form.I18n.Portuguese where

import Yesod.Form.Types (FormMessage (..))
import Data.Monoid (mappend)
import Data.Text (Text)

portugueseFormMessage :: FormMessage -> Text
portugueseFormMessage (MsgInvalidInteger t) = "Número inteiro inválido: " `Data.Monoid.mappend` t
portugueseFormMessage (MsgInvalidNumber t) = "Número inválido: " `mappend` t
portugueseFormMessage (MsgInvalidEntry t) = "Entrada inválida: " `mappend` t
portugueseFormMessage MsgInvalidTimeFormat = "Hora inválida, deve estar no formato HH:MM[:SS]"
portugueseFormMessage MsgInvalidDay = "Data inválida, deve estar no formado AAAA-MM-DD"
portugueseFormMessage (MsgInvalidUrl t) = "URL inválida: " `mappend` t
portugueseFormMessage (MsgInvalidEmail t) = "Endereço de e-mail inválido: " `mappend` t
portugueseFormMessage (MsgInvalidHour t) = "Hora inválida: " `mappend` t
portugueseFormMessage (MsgInvalidMinute t) = "Minutos inválidos: " `mappend` t
portugueseFormMessage (MsgInvalidSecond t) = "Segundos inválidos: " `mappend` t
portugueseFormMessage MsgCsrfWarning = "Como uma proteção contra ataques CSRF, por favor confirme a submissão do seu formulário."
portugueseFormMessage MsgValueRequired = "Preenchimento obrigatório"
portugueseFormMessage (MsgInputNotFound t) = "Entrada não encontrada: " `mappend` t
portugueseFormMessage MsgSelectNone = "<Nenhum>"
portugueseFormMessage (MsgInvalidBool t) = "Booleano inválido: " `mappend` t
portugueseFormMessage MsgBoolYes = "Sim"
portugueseFormMessage MsgBoolNo = "Não"
portugueseFormMessage MsgDelete = "Remover?"
