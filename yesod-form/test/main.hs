{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Data.Time (TimeOfDay (TimeOfDay))
import Data.Text (pack)

import Yesod.Form.Fields (parseTime)
import Yesod.Form.Types

main :: IO ()
main = hspec $
    describe "parseTime" $ mapM_ (\(s, e) -> it s $ parseTime (pack s) `shouldBe` e)
        [ ("01:00:00", Right $ TimeOfDay 1 0 0)
        , ("1:00", Right $ TimeOfDay 1 0 0)
        , ("1:00 AM", Right $ TimeOfDay 1 0 0)
        , ("1:00 am", Right $ TimeOfDay 1 0 0)
        , ("1:00AM", Right $ TimeOfDay 1 0 0)
        , ("1:00am", Right $ TimeOfDay 1 0 0)
        , ("01:00:00am", Right $ TimeOfDay 1 0 0)
        , ("01:00:00 am", Right $ TimeOfDay 1 0 0)
        , ("01:00:00AM", Right $ TimeOfDay 1 0 0)
        , ("01:00:00 AM", Right $ TimeOfDay 1 0 0)
        , ("1:00:01", Right $ TimeOfDay 1 0 1)
        , ("1:00:02 AM", Right $ TimeOfDay 1 0 2)
        , ("1:00:04 am", Right $ TimeOfDay 1 0 4)
        , ("1:00:05 am", Right $ read "01:00:05")
        , ("1:00:64 am", Left $ MsgInvalidSecond "64")
        , ("1:00:4 am", Left $ MsgInvalidSecond "4")
        , ("0:00", Right $ TimeOfDay 0 0 0)
        , ("12:00am", Right $ TimeOfDay 0 0 0)
        , ("12:59:59am", Right $ TimeOfDay 0 59 59)
        , ("12:59:60am", Left $ MsgInvalidSecond "60")
        , ("12:60:59am", Left $ MsgInvalidMinute "60")
        , ("12:00pm", Right $ TimeOfDay 12 0 0)
        , ("12:59:59pm", Right $ TimeOfDay 12 59 59)
        , ("12:59:60pm", Left $ MsgInvalidSecond "60")
        , ("12:60:59pm", Left $ MsgInvalidMinute "60")
        , ("12:7pm", Left $ MsgInvalidMinute "7")
        , ("23:47", Right $ TimeOfDay 23 47 0)
        ]
