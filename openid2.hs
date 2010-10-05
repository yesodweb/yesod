{-# LANGUAGE TypeFamilies, QuasiQuotes #-}
import Yesod
import Web.Authenticate.OpenId2
import Data.Object
import Data.Maybe (fromMaybe)
import Network.HTTP.Enumerator

data OID = OID
mkYesod "OID" [$parseRoutes|
/ RootR GET
/forward ForwardR GET
/complete CompleteR GET
|]

instance Yesod OID where approot _ = "http://localhost:3000"

getRootR = defaultLayout [$hamlet|
%form!action=@ForwardR@
    OpenId: 
    %input!type=text!name=openid_identifier!value="http://"
    %input!type=submit
|]

getForwardR = do
    openid <- runFormGet' $ stringInput "openid_identifier"
    render <- getUrlRender
    url <- liftIO $ getForwardUrl openid $ render CompleteR
    redirectString RedirectTemporary url
    return ()

getCompleteR = do
    params <- reqGetParams `fmap` getRequest
    ident <- liftIO $ authenticate params
    return $ RepPlain $ toContent ident

main = withHttpEnumerator $ basicHandler 3000 OID
