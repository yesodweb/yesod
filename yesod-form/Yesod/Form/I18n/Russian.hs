{-# LANGUAGE OverloadedStrings #-}
module Yesod.Form.I18n.Russian where

import Yesod.Form.Types (FormMessage (..))
import Data.Monoid (mappend)
import Data.Text (Text)

russianFormMessage :: FormMessage -> Text
russianFormMessage (MsgInvalidInteger t) = "Неверно записано целое число: " `Data.Monoid.mappend` t
russianFormMessage (MsgInvalidNumber t) = "Неверный формат числа: " `mappend` t
russianFormMessage (MsgInvalidEntry t) = "Неверный выбор: " `mappend` t
russianFormMessage MsgInvalidTimeFormat = "Неверно указано время, используйте формат ЧЧ:ММ[:СС]"
russianFormMessage MsgInvalidDay = "Неверно указана дата, используйте формат ГГГГ-ММ-ДД"
russianFormMessage (MsgInvalidUrl t) = "Неверно указан URL адрес: " `mappend` t
russianFormMessage (MsgInvalidEmail t) = "Неверно указана электронная почта: " `mappend` t
russianFormMessage (MsgInvalidHour t) = "Неверно указан час: " `mappend` t
russianFormMessage (MsgInvalidMinute t) = "Неверно указаны минуты: " `mappend` t
russianFormMessage (MsgInvalidSecond t) = "Неверно указаны секунды: " `mappend` t
russianFormMessage MsgCsrfWarning = "Для защиты от межсайтовой подделки запросов (CSRF), пожалуйста, подтвердите отправку данных формы."
russianFormMessage MsgValueRequired = "Обязательно к заполнению"
russianFormMessage (MsgInputNotFound t) = "Поле не найдено: " `mappend` t
russianFormMessage MsgSelectNone = "<Не выбрано>"
russianFormMessage (MsgInvalidBool t) = "Неверное логическое значение: " `mappend` t
russianFormMessage MsgBoolYes = "Да"
russianFormMessage MsgBoolNo = "Нет"
russianFormMessage MsgDelete = "Удалить?"
