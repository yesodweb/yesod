{-# LANGUAGE TypeFamilies, QuasiQuotes #-}
import Yesod
import Yesod.Widget
import Yesod.Helpers.Static
import Control.Applicative

data HW = HW { hwStatic :: Static }
mkYesod "HW" [$parseRoutes|
/ RootR GET
/form FormR
/static StaticR Static hwStatic
|]
instance Yesod HW where approot _ = ""
wrapper h = [$hamlet|
#wrapper ^h^
%footer Brought to you by Yesod Widgets&trade;
|]
getRootR = applyLayoutW $ wrapWidget wrapper $ do
    i <- newIdent
    setTitle $ string "Hello Widgets"
    addStyle [$hamlet|\#$string.i${color:red}|]
    addStylesheet $ StaticR $ StaticRoute ["style.css"]
    addStylesheetRemote "http://localhost:3000/static/style2.css"
    addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js"
    addScript $ StaticR $ StaticRoute ["script.js"]
    addBody [$hamlet|
%h1#$string.i$ Welcome to my first widget!!!
%p
    %a!href=@RootR@ Recursive link.
%p
    %a!href=@FormR@ Check out the form.
%p.noscript Your script did not load. :(
|]
    addHead [$hamlet|%meta!keywords=haskell|]

handleFormR = do
    (res, form, enctype) <- runFormPost $ (,,,,)
        <$> requiredField stringField (string "My Field") (string "Some tooltip info") Nothing
        <*> requiredField stringField (string "Another field") (string "") (Just "some default text")
        <*> requiredField intField (string "A number field") (string "some nums") (Just 5)
        <*> requiredField dayField (string "A day field") (string "") Nothing
        <*> boolField (string "A checkbox") (string "") (Just False)
    applyLayoutW $ do
        addStyle [$hamlet|\.tooltip{color:#666;font-style:italic}|]
        flip wrapWidget (fieldsToTable form) $ \h -> [$hamlet|
%form!method=post!enctype=$string.show.enctype$
    %table
        ^h^
        %tr
            %td!colspan=2
                %input!type=submit
    %h3
        Result: $string.show.res$
|]
        setTitle $ string "Form"

main = toWaiApp (HW $ fileLookupDir "static" typeByExt) >>= basicHandler 3000
