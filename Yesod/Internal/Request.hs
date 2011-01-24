{-# LANGUAGE OverloadedStrings #-}
module Yesod.Internal.Request
    ( parseWaiRequest
    ) where

import Yesod.Request
import Control.Arrow (first, (***))
import qualified Network.Wai.Parse as NWP
import Data.Maybe (fromMaybe)
import Yesod.Internal
import qualified Network.Wai as W
import qualified Data.ByteString as S
import System.Random (randomR, newStdGen)
import Web.Cookie (parseCookies)

parseWaiRequest :: W.Request
                -> [(String, String)] -- ^ session
                -> Maybe a
                -> IO Request
parseWaiRequest env session' key' = do
    let gets' = map (bsToChars *** bsToChars)
              $ NWP.parseQueryString $ W.queryString env
    let reqCookie = fromMaybe S.empty $ lookup "Cookie"
                  $ W.requestHeaders env
        cookies' = map (bsToChars *** bsToChars) $ parseCookies reqCookie
        acceptLang = lookup "Accept-Language" $ W.requestHeaders env
        langs = map bsToChars $ maybe [] NWP.parseHttpAccept acceptLang
        langs' = case lookup langKey session' of
                    Nothing -> langs
                    Just x -> x : langs
        langs'' = case lookup langKey cookies' of
                    Nothing -> langs'
                    Just x -> x : langs'
        langs''' = case lookup langKey gets' of
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
