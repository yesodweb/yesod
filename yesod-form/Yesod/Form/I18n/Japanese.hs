{-# LANGUAGE OverloadedStrings #-}
module Yesod.Form.I18n.Japanese where

import Yesod.Form.Types (FormMessage (..))
import Data.Monoid (mappend)
import Data.Text (Text)

japaneseFormMessage :: FormMessage -> Text
japaneseFormMessage (MsgInvalidInteger t) = "無効な整数です: " `Data.Monoid.mappend` t
japaneseFormMessage (MsgInvalidNumber t) = "無効な数値です: " `mappend` t
japaneseFormMessage (MsgInvalidEntry t) = "無効な入力です: " `mappend` t
japaneseFormMessage MsgInvalidTimeFormat = "無効な時刻です。HH:MM[:SS]フォーマットで入力してください"
japaneseFormMessage MsgInvalidDay = "無効な日付です。YYYY-MM-DDフォーマットで入力してください"
japaneseFormMessage (MsgInvalidUrl t) = "無効なURLです: " `mappend` t
japaneseFormMessage (MsgInvalidEmail t) = "無効なメールアドレスです: " `mappend` t
japaneseFormMessage (MsgInvalidHour t) = "無効な時間です: " `mappend` t
japaneseFormMessage (MsgInvalidMinute t) = "無効な分です: " `mappend` t
japaneseFormMessage (MsgInvalidSecond t) = "無効な秒です: " `mappend` t
japaneseFormMessage MsgCsrfWarning = "CSRF攻撃を防ぐため、フォームの入力を確認してください"
japaneseFormMessage MsgValueRequired = "値は必須です"
japaneseFormMessage (MsgInputNotFound t) = "入力が見つかりません: " `mappend` t
japaneseFormMessage MsgSelectNone = "<なし>"
japaneseFormMessage (MsgInvalidBool t) = "無効なbool値です: " `mappend` t
japaneseFormMessage MsgBoolYes = "はい"
japaneseFormMessage MsgBoolNo = "いいえ"
japaneseFormMessage MsgDelete = "削除しますか?"
