> {-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, OverloadedStrings #-}
> import Yesod
> import Control.Applicative ((<$>), (<*>))
> 
> data Session = Session
> mkYesod "Session" [parseRoutes|
> / Root GET POST
> |]
> 
> instance Yesod Session where
>     approot _ = ""
>     clientSessionDuration _ = 1
>
> instance RenderMessage Session FormMessage where
>    renderMessage _ _ = defaultFormMessage
>
> getRoot :: Handler RepHtml
> getRoot = do
>     sess <- getSession
>     hamletToRepHtml [hamlet|
> <form method=post
>     <input type=text name=key
>     <input type=text name=val
>     <input type=submit
> <h1>#{show sess}
> |]
> 
> postRoot :: Handler ()
> postRoot = do
>       (key, val) <- runInputPost $ (,) <$> ireq textField "key" <*> ireq textField "val"
>       setSession key val
>       liftIO $ print (key, val)
>       redirect RedirectTemporary Root
>
> main :: IO ()
> main = warpDebug 3000 Session

> _ignored :: Widget
> _ignored = undefined
