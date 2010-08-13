{-# LANGUAGE TypeFamilies, QuasiQuotes, OverloadedStrings #-}
import Yesod
import Yesod.Widget
import Yesod.Helpers.Static
import Yesod.Form.Jquery
import Yesod.Form.Nic
import Control.Applicative
import qualified Data.ByteString.Lazy as L
import System.Directory
import Data.Digest.Pure.MD5

data HW = HW { hwStatic :: Static }
mkYesod "HW" [$parseRoutes|
/ RootR GET
/form FormR
/static StaticR Static hwStatic
/autocomplete AutoCompleteR GET
|]
instance Yesod HW where
    approot _ = ""
    addStaticContent ext _ content = do
        let fn = (base64md5 content) ++ '.' : ext
        liftIO $ createDirectoryIfMissing True "static/tmp"
        liftIO $ L.writeFile ("static/tmp/" ++ fn) content
        return $ Just $ Right (StaticR $ StaticRoute ["tmp", fn] [], [])

instance YesodNic HW
instance YesodJquery HW
wrapper h = [$hamlet|
#wrapper ^h^
%footer Brought to you by Yesod Widgets&trade;
|]
getRootR = applyLayoutW $ flip wrapWidget wrapper $ do
    i <- newIdent
    setTitle $ string "Hello Widgets"
    addStyle [$cassius|
#$i$
    color:red
|]
    addStylesheet $ StaticR $ StaticRoute ["style.css"] []
    addStylesheetRemote "http://localhost:3000/static/style2.css"
    addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js"
    addScript $ StaticR $ StaticRoute ["script.js"] []
    addBody [$hamlet|
%h1#$i$ Welcome to my first widget!!!
%p
    %a!href=@RootR@ Recursive link.
%p
    %a!href=@FormR@ Check out the form.
%p.noscript Your script did not load. :(
|]
    addHead [$hamlet|%meta!keywords=haskell|]

handleFormR = do
    (res, form, enctype) <- runFormPost $ fieldsToTable $ (,,,,,,,,,)
        <$> stringField (FormFieldSettings "My Field" "Some tooltip info" Nothing Nothing) Nothing
        <*> stringField ("Another field") (Just "some default text")
        <*> intField (FormFieldSettings "A number field" "some nums" Nothing Nothing) (Just 5)
        <*> jqueryDayField ("A day field") Nothing
        <*> timeField ("A time field") Nothing
        <*> jqueryDayTimeField ("A day/time field") Nothing
        <*> boolField FormFieldSettings
                { ffsLabel = "A checkbox"
                , ffsTooltip = ""
                , ffsId = Nothing
                , ffsName = Nothing
                } (Just False)
        <*> jqueryAutocompleteField AutoCompleteR
            (FormFieldSettings "Autocomplete" "Try it!" Nothing Nothing) Nothing
        <*> nicHtmlField ("HTML")
                (Just $ string "You can put rich text here")
        <*> maybeEmailField ("An e-mail addres") Nothing
    let mhtml = case res of
                    FormSuccess (_, _, _, _, _, _, _, _, x, _) -> Just x
                    _ -> Nothing
    applyLayoutW $ do
        addStyle [$cassius|
.tooltip
    color:#666
    font-style:italic
|]
        addStyle [$cassius|
textarea.html
    width:300px
    height:150px
|]
        wrapWidget form $ \h -> [$hamlet|
%form!method=post!enctype=$enctype$
    %table
        ^h^
        %tr
            %td!colspan=2
                %input!type=submit
    $maybe mhtml html
        $html$
|]
        setTitle $ string "Form"

main = basicHandler 3000 $ HW $ fileLookupDir "static" typeByExt

getAutoCompleteR :: Handler HW RepJson
getAutoCompleteR = do
    term <- runFormGet' $ stringInput "term"
    jsonToRepJson $ jsonList
        [ jsonScalar $ term ++ "foo"
        , jsonScalar $ term ++ "bar"
        , jsonScalar $ term ++ "baz"
        ]
