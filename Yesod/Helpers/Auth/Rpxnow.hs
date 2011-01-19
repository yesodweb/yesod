{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}
module Yesod.Helpers.Auth.Rpxnow
    ( authRpxnow
    ) where

import Yesod.Helpers.Auth
import qualified Web.Authenticate.Rpxnow as Rpxnow
import Control.Monad (mplus)

import Yesod.Handler
import Yesod.Widget
import Yesod.Request
import Text.Hamlet (hamlet)
import Control.Monad.IO.Class (liftIO)

authRpxnow :: YesodAuth m
           => String -- ^ app name
           -> String -- ^ key
           -> AuthPlugin m
authRpxnow app apiKey =
    AuthPlugin "rpxnow" dispatch login
  where
    login tm = do
        let url = {- FIXME urlEncode $ -} tm $ PluginR "rpxnow" []
        addHamlet
#if GHC7
            [hamlet|
#else
            [$hamlet|
#endif
<iframe src="http://#{app}.rpxnow.com/openid/embed?token_url=@{url}" scrolling="no" frameBorder="no" allowtransparency="true" style="width:400px;height:240px">
|]
    dispatch _ [] = do
        token1 <- lookupGetParam "token"
        token2 <- lookupPostParam "token"
        let token = case token1 `mplus` token2 of
                        Nothing -> invalidArgs ["token: Value not supplied"]
                        Just x -> x
        Rpxnow.Identifier ident extra <- liftIO $ Rpxnow.authenticate apiKey token
        let creds =
                Creds "rpxnow" ident
                $ maybe id (\x -> (:) ("verifiedEmail", x))
                    (lookup "verifiedEmail" extra)
                $ maybe id (\x -> (:) ("displayName", x))
                    (getDisplayName extra)
                  []
        setCreds True creds
    dispatch _ _ = notFound

-- | Get some form of a display name.
getDisplayName :: [(String, String)] -> Maybe String
getDisplayName extra =
    foldr (\x -> mplus (lookup x extra)) Nothing choices
  where
    choices = ["verifiedEmail", "email", "displayName", "preferredUsername"]
