> {-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, OverloadedStrings #-}
> import Yesod
> import Control.Applicative ((<$>), (<*>))
> 
> data Session = Session
> type Handler = GHandler Session Session
> mkYesod "Session" [$parseRoutes|
> / Root GET POST
> |]
> getRoot :: Handler RepHtml
> getRoot = do
>     sess <- getSession
>     hamletToRepHtml [$hamlet|
> <form method=post
>     <input type=text name=key
>     <input type=text name=val
>     <input type=submit
> <h1>#{show sess}
> |]
> 
> postRoot :: Handler ()
> postRoot = do
>     (key, val) <- runFormPost' $ (,) <$> stringInput "key" <*> stringInput "val"
>     setSession key val
>     liftIO $ print (key, val)
>     redirect RedirectTemporary Root
> 
> instance Yesod Session where
>     approot _ = ""
>     clientSessionDuration _ = 1
> main = warpDebug 3000 Session
