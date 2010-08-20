import Yesod.Mail
import qualified Data.ByteString.Lazy.Char8 as L
import System.Environment

main = do
    [dest] <- getArgs
    let p1 = Part "text/html" None Inline $ L.pack "<h1>Hello World!!!</h1>"
    lbs <- L.readFile "mail.hs"
    let p2 = Part "text/plain" Base64 (Attachment "mail.hs") lbs
    let mail = Mail
            [("To", dest), ("Subject", "mail quine")]
            "Plain stuff. Mime-clients should not show it."
            [p1, p2]
    renderSendMail mail
