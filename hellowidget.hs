{-# LANGUAGE TypeFamilies, QuasiQuotes, OverloadedStrings #-}
import Yesod
import Yesod.Widget
import Yesod.Helpers.Static
import Control.Applicative

data HW = HW { hwStatic :: Static }
mkYesod "HW" [$parseRoutes|
/ RootR GET
/form FormR
/static StaticR Static hwStatic
/autocomplete AutoCompleteR GET
|]
instance Yesod HW where approot _ = ""
wrapper h = [$hamlet|
#wrapper ^h^
%footer Brought to you by Yesod Widgets&trade;
|]
getRootR = applyLayoutW $ flip wrapWidget wrapper $ do
    i <- newIdent
    setTitle $ string "Hello Widgets"
    addStyle [$hamlet|\#$i${color:red}|]
    addStylesheet $ StaticR $ StaticRoute ["style.css"]
    addStylesheetRemote "http://localhost:3000/static/style2.css"
    addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js"
    addScript $ StaticR $ StaticRoute ["script.js"]
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
    (res, form, enctype) <- runFormPost $ (,,,,,,,,)
        <$> stringField (string "My Field") (string "Some tooltip info") Nothing
        <*> stringField (string "Another field") (string "") (Just "some default text")
        <*> intField (string "A number field") (string "some nums") (Just 5)
        <*> jqueryDayField (string "A day field") (string "") Nothing
        <*> timeField (string "A time field") (string "") Nothing
        <*> boolField FormFieldSettings
                { ffsLabel = "A checkbox"
                , ffsTooltip = ""
                , ffsId = Nothing
                , ffsName = Nothing
                } (Just False)
        <*> jqueryAutocompleteField AutoCompleteR
            (string "Autocomplete") (string "Try it!") Nothing
        <*> nicHtmlField (string "HTML") (string "")
                (Just $ string "You can put rich text here")
        <*> maybeEmailField (string "An e-mail addres") mempty Nothing
    let mhtml = case res of
                    FormSuccess (_, _, _, _, _, _, _, x, _) -> Just x
                    _ -> Nothing
    applyLayoutW $ do
        addStyle [$hamlet|\.tooltip{color:#666;font-style:italic}|]
        addStyle [$hamlet|textarea.html{width:300px;height:150px};|]
        wrapWidget (fieldsToTable form) $ \h -> [$hamlet|
%form!method=post!enctype=$show.enctype$
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
        [ jsonScalar $ string $ term ++ "foo"
        , jsonScalar $ string $ term ++ "bar"
        , jsonScalar $ string $ term ++ "baz"
        ]
