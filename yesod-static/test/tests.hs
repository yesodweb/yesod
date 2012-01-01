{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import YesodStaticTest (specs)

main :: IO ()
main = hspecX $ descriptions specs
