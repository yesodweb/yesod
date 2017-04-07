{-# LANGUAGE OverloadedStrings #-}
module Yesod.Form.I18n.Chinese where

import Yesod.Form.Types (FormMessage (..))
import Data.Monoid (mappend)
import Data.Text (Text)

chineseFormMessage :: FormMessage -> Text
chineseFormMessage (MsgInvalidInteger t) = "无效的整数: " `Data.Monoid.mappend` t
chineseFormMessage (MsgInvalidNumber t) = "无效的数字: " `mappend` t
chineseFormMessage (MsgInvalidEntry t) = "无效的条目: " `mappend` t
chineseFormMessage MsgInvalidTimeFormat = "无效的时间, 必须符合HH:MM[:SS]格式"
chineseFormMessage MsgInvalidDay = "无效的日期, 必须符合YYYY-MM-DD格式"
chineseFormMessage (MsgInvalidUrl t) = "无效的链接: " `mappend` t
chineseFormMessage (MsgInvalidEmail t) = "无效的邮箱地址: " `mappend` t
chineseFormMessage (MsgInvalidHour t) = "无效的小时: " `mappend` t
chineseFormMessage (MsgInvalidMinute t) = "无效的分钟: " `mappend` t
chineseFormMessage (MsgInvalidSecond t) = "无效的秒: " `mappend` t
chineseFormMessage MsgCsrfWarning = "为了防备跨站请求伪造, 请确认表格提交."
chineseFormMessage MsgValueRequired = "此项必填"
chineseFormMessage (MsgInputNotFound t) = "输入找不到: " `mappend` t
chineseFormMessage MsgSelectNone = "<空>"
chineseFormMessage (MsgInvalidBool t) = "无效的逻辑值: " `mappend` t
chineseFormMessage MsgBoolYes = "是"
chineseFormMessage MsgBoolNo = "否"
chineseFormMessage MsgDelete = "删除?"
