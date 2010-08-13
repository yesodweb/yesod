{-# LANGUAGE QuasiQuotes #-}
module Yesod.Form.Profiles
    ( FieldProfile (..)
    , stringFieldProfile
    , textareaFieldProfile
    , hiddenFieldProfile
    , intFieldProfile
    , dayFieldProfile
    , timeFieldProfile
    , htmlFieldProfile
    , emailFieldProfile
    , urlFieldProfile
    , doubleFieldProfile
    , parseDate
    , parseTime
    ) where

import Yesod.Form.Core
import Text.Hamlet
import Data.Time (Day, TimeOfDay(..))
import qualified Data.ByteString.Lazy.UTF8 as U
import qualified Text.Email.Validate as Email
import Network.URI (parseURI)

intFieldProfile :: Integral i => FieldProfile sub y i
intFieldProfile = FieldProfile
    { fpParse = maybe (Left "Invalid integer") Right . readMayI
    , fpRender = showI
    , fpHamlet = \theId name val isReq -> [$hamlet|
%input#$theId$!name=$name$!type=number!:isReq:required!value=$val$
|]
    , fpWidget = \_name -> return ()
    }
  where
    showI x = show (fromIntegral x :: Integer)
    readMayI s = case reads s of
                    (x, _):_ -> Just $ fromInteger x
                    [] -> Nothing

doubleFieldProfile :: FieldProfile sub y Double
doubleFieldProfile = FieldProfile
    { fpParse = maybe (Left "Invalid number") Right . readMay
    , fpRender = show
    , fpHamlet = \theId name val isReq -> [$hamlet|
%input#$theId$!name=$name$!type=number!:isReq:required!value=$val$
|]
    , fpWidget = \_name -> return ()
    }

dayFieldProfile :: FieldProfile sub y Day
dayFieldProfile = FieldProfile
    { fpParse = parseDate
    , fpRender = show
    , fpHamlet = \theId name val isReq -> [$hamlet|
%input#$theId$!name=$name$!type=date!:isReq:required!value=$val$
|]
    , fpWidget = const $ return ()
    }

timeFieldProfile :: FieldProfile sub y TimeOfDay
timeFieldProfile = FieldProfile
    { fpParse = parseTime
    , fpRender = show
    , fpHamlet = \theId name val isReq -> [$hamlet|
%input#$theId$!name=$name$!:isReq:required!value=$val$
|]
    , fpWidget = const $ return ()
    }

htmlFieldProfile :: FieldProfile sub y Html
htmlFieldProfile = FieldProfile
    { fpParse = Right . preEscapedString
    , fpRender = U.toString . renderHtml
    , fpHamlet = \theId name val _isReq -> [$hamlet|
%textarea.html#$theId$!name=$name$ $val$
|]
    , fpWidget = const $ return ()
    }

textareaFieldProfile :: FieldProfile sub y String
textareaFieldProfile = FieldProfile
    { fpParse = Right
    , fpRender = id
    , fpHamlet = \theId name val _isReq -> [$hamlet|
%textarea#$theId$!name=$name$ $val$
|]
    , fpWidget = const $ return ()
    }

hiddenFieldProfile :: FieldProfile sub y String
hiddenFieldProfile = FieldProfile
    { fpParse = Right
    , fpRender = id
    , fpHamlet = \theId name val _isReq -> [$hamlet|
%input!type=hidden#$theId$!name=$name$!value=$val$
|]
    , fpWidget = const $ return ()
    }

stringFieldProfile :: FieldProfile sub y String
stringFieldProfile = FieldProfile
    { fpParse = Right
    , fpRender = id
    , fpHamlet = \theId name val isReq -> [$hamlet|
%input#$theId$!name=$name$!type=text!:isReq:required!value=$val$
|]
    , fpWidget = \_name -> return ()
    }

readMay :: Read a => String -> Maybe a
readMay s = case reads s of
                (x, _):_ -> Just x
                [] -> Nothing

parseDate :: String -> Either String Day
parseDate = maybe (Left "Invalid day, must be in YYYY-MM-DD format") Right
              . readMay . replace '/' '-'

-- | Replaces all instances of a value in a list by another value.
-- from http://hackage.haskell.org/packages/archive/cgi/3001.1.7.1/doc/html/src/Network-CGI-Protocol.html#replace
replace :: Eq a => a -> a -> [a] -> [a]
replace x y = map (\z -> if z == x then y else z)

parseTime :: String -> Either String TimeOfDay
parseTime (h2:':':m1:m2:[]) = parseTimeHelper ('0', h2, m1, m2, '0', '0')
parseTime (h1:h2:':':m1:m2:[]) = parseTimeHelper (h1, h2, m1, m2, '0', '0')
parseTime (h1:h2:':':m1:m2:' ':'A':'M':[]) =
    parseTimeHelper (h1, h2, m1, m2, '0', '0')
parseTime (h1:h2:':':m1:m2:' ':'P':'M':[]) =
    let [h1', h2'] = show $ (read [h1, h2] :: Int) + 12
    in parseTimeHelper (h1', h2', m1, m2, '0', '0')
parseTime (h1:h2:':':m1:m2:':':s1:s2:[]) =
    parseTimeHelper (h1, h2, m1, m2, s1, s2)
parseTime _ = Left "Invalid time, must be in HH:MM[:SS] format"

parseTimeHelper :: (Char, Char, Char, Char, Char, Char)
                -> Either [Char] TimeOfDay
parseTimeHelper (h1, h2, m1, m2, s1, s2)
    | h < 0 || h > 23 = Left $ "Invalid hour: " ++ show h
    | m < 0 || m > 59 = Left $ "Invalid minute: " ++ show m
    | s < 0 || s > 59 = Left $ "Invalid second: " ++ show s
    | otherwise = Right $ TimeOfDay h m s
  where
    h = read [h1, h2]
    m = read [m1, m2]
    s = fromInteger $ read [s1, s2]

emailFieldProfile :: FieldProfile s y String
emailFieldProfile = FieldProfile
    { fpParse = \s -> if Email.isValid s
                        then Right s
                        else Left "Invalid e-mail address"
    , fpRender = id
    , fpHamlet = \theId name val isReq -> [$hamlet|
%input#$theId$!name=$name$!type=email!:isReq:required!value=$val$
|]
    , fpWidget = const $ return ()
    }

urlFieldProfile :: FieldProfile s y String
urlFieldProfile = FieldProfile
    { fpParse = \s -> case parseURI s of
                        Nothing -> Left "Invalid URL"
                        Just _ -> Right s
    , fpRender = id
    , fpHamlet = \theId name val isReq -> [$hamlet|
%input#$theId$!name=$name$!type=url!:isReq:required!value=$val$
|]
    , fpWidget = const $ return ()
    }
