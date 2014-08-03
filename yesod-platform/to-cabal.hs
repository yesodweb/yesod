import Data.List (intercalate, isPrefixOf)
import Control.Applicative ((<$>))

main = do
    pkgs <- map (intercalate " == ")
          . filter (\xs -> not $ any (`isPrefixOf` xs) $ map return ["parsec", "text", "transformers", "mtl", "HUnit", "QuickCheck", "binary", "zlib", "stm", "regex-compat", "hashable", "vault", "integer-gmp", "unordered-containers", "async", "aeson", "attoparsec", "scientific", "case-insensitive", "vector", "primitive", "unix-compat"])
          . map words
          . filter (not . null)
          . lines
        <$> getContents
    putStrLn "name:            yesod-platform"
    putStrLn "version:         FIXME"
    putStrLn "license:         MIT"
    putStrLn "license-file:    LICENSE"
    putStrLn "author:          Michael Snoyman <michael@snoyman.com>"
    putStrLn "maintainer:      Michael Snoyman <michael@snoyman.com>"
    putStrLn "synopsis:        Meta package for Yesod"
    putStrLn "description:     Instead of allowing version ranges of dependencies, this package requires specific versions to avoid dependency hell"
    putStrLn "category:        Web, Yesod"
    putStrLn "stability:       Stable"
    putStrLn "cabal-version:   >= 1.6"
    putStrLn "build-type:      Simple"
    putStrLn "homepage:        http://www.yesodweb.com/"
    putStrLn ""
    putStrLn "library"
    putStrLn "    build-depends: base >= 4 && < 5"
    mapM_ go pkgs
    putStrLn ""
    putStrLn "    exposed-modules: Yesod.Platform"
    putStrLn ""
    putStrLn "source-repository head"
    putStrLn "  type:     git"
    putStrLn "  location: https://github.com/yesodweb/yesod"

go s = putStrLn $ concat ["                 , ", s]
