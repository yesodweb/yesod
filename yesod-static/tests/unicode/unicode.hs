import System.Directory
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as S8

main = getDirectoryContents "." >>= mapM_ putStrLn . map fix

fix = T.unpack . TE.decodeUtf8 . S8.pack
