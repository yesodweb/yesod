module Web.Authenticate.Internal
    ( qsEncode
    ) where

import Codec.Binary.UTF8.String (encode)
import Numeric (showHex)

qsEncode :: String -> String
qsEncode =
    concatMap go . encode
  where
    go 32 = "+" -- space
    go 46 = "."
    go 45 = "-"
    go 126 = "~"
    go 95 = "_"
    go c
        | 48 <= c && c <= 57 = [w2c c]
        | 65 <= c && c <= 90 = [w2c c]
        | 97 <= c && c <= 122 = [w2c c]
    go c = '%' : showHex c ""
    w2c = toEnum . fromEnum
