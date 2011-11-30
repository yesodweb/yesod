> {-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, OverloadedStrings #-}

> import Yesod
> import qualified Data.ByteString.Char8 as S8
> import qualified Data.Text as T

> data Echo = Echo

> mkYesod "Echo" [parseRoutes|
> / Homepage GET POST
> |]

> instance Yesod Echo where approot _ = ""

> getHomepage :: Handler RepHtml
> getHomepage = defaultLayout $ do
>   setTitle "Upload a file"
>   addHamlet [hamlet|
> <form method=post action=. enctype=multipart/form-data>
>   File name:
>   <input type=file name=file
>   <input type=submit
> |]

> postHomepage :: Handler [(ContentType, Content)]
> postHomepage = do
>   (_, files) <- runRequestBody
>   fi <- maybe notFound return $ lookup "file" files
>   return [(S8.pack $ T.unpack $ fileContentType fi, toContent $ fileContent fi)]

> main :: IO ()
> main = warpDebug 3000 Echo

To avoid warnings

> _ignored :: Widget
> _ignored = undefined
