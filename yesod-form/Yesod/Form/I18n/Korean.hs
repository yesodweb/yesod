{-# LANGUAGE OverloadedStrings #-}
module Yesod.Form.I18n.Korean where

import Yesod.Form.Types (FormMessage (..))
import Data.Monoid (mappend)
import Data.Text (Text)

koreanFormMessage :: FormMessage -> Text
koreanFormMessage (MsgInvalidInteger t) = "잘못된 정수입니다: " `Data.Monoid.mappend` t
koreanFormMessage (MsgInvalidNumber t) = "잘못된 숫자입니다: " `mappend` t
koreanFormMessage (MsgInvalidEntry t) = "잘못된 입력입니다: " `mappend` t
koreanFormMessage MsgInvalidTimeFormat = "잘못된 시간입니다. HH:MM[:SS] 형태로 입력하세요"
koreanFormMessage MsgInvalidDay = "잘못된 날짜입니다. YYYY-MM-DD 형태로 입력하세요"
koreanFormMessage (MsgInvalidUrl t) = "잘못된 URL입니다: " `mappend` t
koreanFormMessage (MsgInvalidEmail t) = "잘못된 이메일 주소입니다: " `mappend` t
koreanFormMessage (MsgInvalidHour t) = "잘못된 시간입니다: " `mappend` t
koreanFormMessage (MsgInvalidMinute t) = "잘못된 분입니다: " `mappend` t
koreanFormMessage (MsgInvalidSecond t) = "잘못된 초입니다: " `mappend` t
koreanFormMessage MsgCsrfWarning = "CSRF공격을 방지하기 위해 양식의 입력을 확인하세요."
koreanFormMessage MsgValueRequired = "값은 필수입니다"
koreanFormMessage (MsgInputNotFound t) = "입력을 찾을 수 없습니다: " `mappend` t
koreanFormMessage MsgSelectNone = "<없음>"
koreanFormMessage (MsgInvalidBool t) = "잘못된 불(boolean)입니다: " `mappend` t
koreanFormMessage MsgBoolYes = "예"
koreanFormMessage MsgBoolNo = "아니오"
koreanFormMessage MsgDelete = "삭제하시겠습니까?"
