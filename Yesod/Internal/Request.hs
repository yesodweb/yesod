{-# LANGUAGE OverloadedStrings #-}
module Yesod.Internal.Request
    ( parseWaiRequest
    ) where

import Yesod.Request
import Control.Arrow (first, (***))
import qualified Network.Wai.Parse as NWP
import Yesod.Internal
import qualified Network.Wai as W
import System.Random (randomR, newStdGen)
import Web.Cookie (parseCookies)
import qualified Data.Ascii as A
import Data.Monoid (mempty)

parseWaiRequest :: W.Request
                -> [(String, String)] -- ^ session
                -> Maybe a
                -> IO Request
parseWaiRequest env session' key' = do
    let gets' = map (bsToChars *** maybe "" bsToChars)
              $ W.queryString env
    let reqCookie = maybe mempty id $ lookup "Cookie"
                  $ W.requestHeaders env
        cookies' = parseCookies reqCookie
        acceptLang = lookup "Accept-Language" $ W.requestHeaders env
        langs = map A.toString $ maybe [] NWP.parseHttpAccept acceptLang
        langs' = case lookup (A.toString langKey) session' of
                    Nothing -> langs
                    Just x -> x : langs
        langs'' = case lookup langKey cookies' of
                    Nothing -> langs'
                    Just x -> A.toString x : langs'
        langs''' = case lookup (A.toString langKey) gets' of
                     Nothing -> langs''
                     Just x -> x : langs''
    nonce <- case (key', lookup nonceKey session') of
                (Nothing, _) -> return Nothing
                (_, Just x) -> return $ Just x
                (_, Nothing) -> do
                    g <- newStdGen
                    return $ Just $ fst $ randomString 10 g
    return $ Request gets' cookies' env langs''' nonce
  where
    randomString len =
        first (map toChar) . sequence' (replicate len (randomR (0, 61)))
    sequence' [] g = ([], g)
    sequence' (f:fs) g =
        let (f', g') = f g
            (fs', g'') = sequence' fs g'
         in (f' : fs', g'')
    toChar i
        | i < 26 = toEnum $ i + fromEnum 'A'
        | i < 52 = toEnum $ i + fromEnum 'a' - 26
        | otherwise = toEnum $ i + fromEnum '0' - 52
