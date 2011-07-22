{-# LANGUAGE TemplateHaskell #-}
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp (run)
import Data.FileEmbed

main :: IO ()
main = run 3000 $ staticApp defaultStaticSettings
    { ssFolder = embeddedLookup $ toEmbedded $(embedDir ".")
    , ssIndices = []
    , ssMaxAge = NoMaxAge
    }
