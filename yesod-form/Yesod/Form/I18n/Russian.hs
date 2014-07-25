{-# LANGUAGE OverloadedStrings #-}
module Yesod.Form.I18n.English where

import Yesod.Form.Types (FormMessage (..))
import Data.Monoid (mappend)
import Data.Text (Text)

englishFormMessage :: FormMessage -> Text
englishFormMessage (MsgInvalidInteger t) = "Неверно записано целое число: " `mappend` t
englishFormMessage (MsgInvalidNumber t) = "Неверный формат числа: " `mappend` t
englishFormMessage (MsgInvalidEntry t) = "Неверный выбор: " `mappend` t
englishFormMessage MsgInvalidTimeFormat = "Неверно указано время, используйте формат ЧЧ:ММ[:СС]"
englishFormMessage MsgInvalidDay = "Неверно указана дата, используйте формат ГГГГ-ММ-ДД"
englishFormMessage (MsgInvalidUrl t) = "Неверно указан URL адрес: " `mappend` t
englishFormMessage (MsgInvalidEmail t) = "Неверно указана электронная почта: " `mappend` t
englishFormMessage (MsgInvalidHour t) = "Неверно указан час: " `mappend` t
englishFormMessage (MsgInvalidMinute t) = "Неверно указаны минуты: " `mappend` t
englishFormMessage (MsgInvalidSecond t) = "Неверны указаны секунды: " `mappend` t
englishFormMessage MsgCsrfWarning = "Для защиты от межсайтовой подделки запросов (CSRF), пожалуйста, подтвердите отправку данных формы."
englishFormMessage MsgValueRequired = "Отсутствует требуемое значение."
englishFormMessage (MsgInputNotFound t) = "Поле не найдено: " `mappend` t
englishFormMessage MsgSelectNone = "<Не выбрано>"
englishFormMessage (MsgInvalidBool t) = "Неверное логическое значение: " `mappend` t
englishFormMessage MsgBoolYes = "Да"
englishFormMessage MsgBoolNo = "Нет"
englishFormMessage MsgDelete = "Удалить?"
