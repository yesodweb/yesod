{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import YesodStaticTest (specs)
import EmbedProductionTest (embedProductionSpecs)
import EmbedDevelTest (embedDevSpecs)

main :: IO ()
main = hspec $ do
    specs
    embedProductionSpecs
    embedDevSpecs
