{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies #-}
-- | This embeds just a single file; it embeds the source code file 
-- \"sample-embed.hs\" from the current directory so when you compile,
-- the sample-embed.hs file must be in the current directory.
--
-- Try toggling the development argument to 'mkEmbeddedStatic'. When the
-- development argument is true the file \"sample-embed.hs\" is reloaded
-- from disk on every request (try changing it after you start the server).
-- When development is false, the contents are embedded and the sample-embed.hs
-- file does not even need to be present during runtime.
module Main where

import Yesod.Core
import Yesod.EmbeddedStatic

mkEmbeddedStatic False "eStatic" [embedFile "sample-embed.hs"]

-- The above will generate variables
-- eStatic :: EmbeddedStatic
-- sample_embed_hs :: Route EmbeddedStatic

data MyApp = MyApp { getStatic :: EmbeddedStatic }

mkYesod "MyApp" [parseRoutes|
/ HomeR GET
/static StaticR EmbeddedStatic getStatic
|]

instance Yesod MyApp where
     addStaticContent = embedStaticContent getStatic StaticR Right

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    toWidget [julius|console.log("Hello World");|]
    [whamlet|
<h1>Hello
<p>Check the
    <a href=@{StaticR sample_embed_hs}>embedded file
|]

main :: IO ()
main = warp 3000 $ MyApp eStatic
