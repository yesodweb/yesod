#!/usr/bin/env runhaskell

> module Main where
> import Distribution.Simple
> import System.Cmd (system)

> main :: IO ()
> main = defaultMainWithHooks (simpleUserHooks { runTests = runTests' })

> runTests' :: a -> b -> c -> d -> IO ()
> runTests' _ _ _ _ = system "runhaskell -DTEST runtests.hs" >> return ()
