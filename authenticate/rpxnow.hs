{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, OverloadedStrings #-}
import Yesod
import Web.Authenticate.Rpxnow
import Data.Maybe (fromMaybe)
import qualified Data.Aeson as A
import qualified Data.Vector as V
import qualified Data.Map as M
import Data.Text (unpack)

appName :: String
appName = "yesod-test"
apiKey = "c8043882f14387d7ad8dfc99a1a8dab2e028f690"
data RP = RP
type Handler = GHandler RP RP

mkYesod "RP" [parseRoutes|
/ RootR GET
/complete CompleteR POST
|]

instance Yesod RP where approot _ = "http://localhost:3000"

getRootR :: Handler RepHtml
getRootR = defaultLayout [hamlet|
<iframe src="http://#{appName}.rpxnow.com/openid/embed?token_url=@{CompleteR}" scrolling="no" frameBorder="no" allowtransparency="true" style="width:400px;height:240px">
|]

postCompleteR :: Handler RepHtml
postCompleteR = do
    Just token <- lookupPostParam "token"
    Identifier ident extra <- liftIO $ authenticate apiKey $ unpack token
    defaultLayout [hamlet|
<h1>Ident: #{ident}
<h2>Extra: #{show $ extra}
|]

main :: IO ()
main = warpDebug 3000 RP
