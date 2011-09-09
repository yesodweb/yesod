> {-# LANGUAGE TypeFamilies, QuasiQuotes, OverloadedStrings, MultiParamTypeClasses, TemplateHaskell #-}
> import Yesod
> import Yesod.Static
> import Yesod.Form.Jquery
> import Yesod.Form.Nic
> import Control.Applicative
> import Data.Text (unpack)
> import Text.Blaze (string)
> 
> data HW = HW { hwStatic :: Static }
> mkYesod "HW" [$parseRoutes|
> / RootR GET
> /form FormR
> /static StaticR Static hwStatic
> /autocomplete AutoCompleteR GET
> |]
> instance Yesod HW where approot _ = ""
> instance YesodJquery HW
> instance YesodNic HW
> wrapper h = [hamlet|
> <#wrapper>^{h}
> <footer>Brought to you by Yesod Widgets&trade;
> |]
> instance RenderMessage HW FormMessage where
>    renderMessage _ _ = defaultFormMessage
>
> getRootR = defaultLayout $ wrapper $ do
>     i <- lift newIdent
>     setTitle $ string "Hello Widgets"
>     addCassius [$cassius|
>   #$i$
>       color: red|]
>     addStylesheet $ StaticR $ StaticRoute ["style.css"] []
>     addStylesheetRemote "http://localhost:3000/static/style2.css"
>     addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js"
>     addScript $ StaticR $ StaticRoute ["script.js"] []
>     addHamlet [$hamlet|
> <h1 ##{i}>Welcome to my first widget!!!
> <p
>     <a href=@RootR@>Recursive link.
> <p
>     <a href=@FormR@>Check out the form.
> <p .noscript>Your script did not load. :(
> |]
>     addHtmlHead [$hamlet|<meta keywords=haskell|]
> 
> handleFormR = do
>     (res, form, enctype, nonce) <- runFormPost $ renderTable $ (,,,,,,,,)
>         <$> aopt textField "My Field" Nothing
>         <*> aopt textField "Another field" (Just "some default text")
>         <*> aopt intField "A number field" (Just 5)
>         <*> aopt jqueryDayField "A day field" Nothing
>         <*> aopt timeField "A time field" Nothing
>         <*> aopt boolField "A checkbox" (Just False)
>         <*> aopt jqueryAutocompleteField AutoCompleteR "Autocomplete" Nothing
>         <*> aopt nicHtmlField "HTML"
>                 (Just $ string "You can put <rich text> here")
>         <*> aopt emailField "An e-mail addres" Nothing
>     let mhtml = case res of
>                     FormSuccess (_, _, _, _, _, _, _, x, _) -> Just x
>                     _ -> Nothing
>     defaultLayout $ do
>         addCassius [$cassius|
> .tooltip
>     color: #666
>     font-style: italic
> textarea.html
>     width: 300px
>     height: 150px|]
>         addWidget [$hamlet|
> <form method="post" enctype="#{enctype}">
>     <table>
>         \^{form}
>         <tr>
>             <td colspan="2">
>                 \#{nonce}
>                 <input type="submit">
>     $maybe html <- mhtml
>         \#{html}
> |]
>         setTitle $ string "Form"
> 
> main = static "static" >>= (warpDebug 3000 . HW)
> 
> getAutoCompleteR :: Handler RepJson
> getAutoCompleteR = do
>     term <- runInputGet $ ireq textField "term"
>     jsonToRepJson $ jsonList
>         [ jsonScalar $ unpack term ++ "foo"
>         , jsonScalar $ unpack term ++ "bar"
>         , jsonScalar $ unpack term ++ "baz"
>         ]
