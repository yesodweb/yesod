> {-# LANGUAGE QuasiQuotes #-}
> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE CPP #-}

> import Yesod
> import Data.Text (Text)

> data I18N = I18N

> mkYesod "I18N" [parseRoutes|
> /            HomepageR GET
> /set/#Text SetLangR  GET
> |]

> instance Yesod I18N where
>     approot _ = "http://localhost:3000"

> getHomepageR :: Handler RepHtml
> getHomepageR = do
>     ls <- languages
>     let hello = chooseHello ls
>     let choices =
>             [ ("en", "English") :: (Text, Text)
>             , ("es", "Español")
>             , ("he", "עִבְרִית")
>             ]
>     defaultLayout $ do
>       setTitle "I18N Homepage"
>       addHamlet [hamlet|
> <h1>#{hello}
> <p>In other languages:
> <ul>
>     $forall choice <- choices
>         <li>
>             <a href="@{SetLangR (fst choice)}">#{snd choice}
> |]

> chooseHello :: [Text] -> Text
> chooseHello [] = "Hello"
> chooseHello ("he":_) = "שלום"
> chooseHello ("es":_) = "¡Hola!"
> chooseHello (_:rest) = chooseHello rest

> getSetLangR :: Text -> Handler ()
> getSetLangR lang = do
>     setLanguage lang
>     redirect RedirectTemporary HomepageR

> main :: IO ()
> main = warpDebug 3000 I18N

> _ignored :: Widget
> _ignored = undefined
