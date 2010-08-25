{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
import CodeGenQ
import System.IO
import System.Directory
import qualified Data.ByteString.Char8 as S
import Language.Haskell.TH.Syntax

main :: IO ()
main = do
    putStr [$codegen|Welcome to the Yesod scaffolder.
I'm going to be creating a skeleton Yesod project for you.

What is your name? We're going to put this in the cabal and LICENSE files.

Your name: |]
    hFlush stdout
    name <- getLine

    putStr [$codegen|
Welcome ~name~.
What do you want to call your project? We'll use this for the cabal name.

Project name: |]
    hFlush stdout
    project <- getLine

    putStr [$codegen|
Now where would you like me to place your generated files? I'm smart enough
to create the directories, don't worry about that. If you leave this answer
blank, we'll place the files in ~project~.

Directory name: |]
    hFlush stdout
    dirRaw <- getLine
    let dir = if null dirRaw then project else dirRaw

    putStr [$codegen|
Great, we'll be creating ~project~ today, and placing it in ~dir~.
What's going to be the name of your site argument datatype? This name must
start with a capital letter.

Site argument: |]
    hFlush stdout
    sitearg <- getLine

    putStr [$codegen|
That's it! I'm creating your files now...
|]

    putStr [$codegen|
Yesod uses Persistent for its (you guessed it) persistence layer.
This tool will build in either SQLite or PostgreSQL support for you. If you
want to use a different backend, you'll have to make changes manually.
If you're not sure, stick with SQLite: it has no dependencies.

So, what'll it be? s for sqlite, p for postgresql: |]
    hFlush stdout
    backendS <- getLine
    let pconn1 = [$codegen|user=~project~ password=~project~ host=localhost port=5432 dbname=~project~_debug|]
    let pconn2 = [$codegen|user=~project~ password=~project~ host=localhost port=5432 dbname=~project~_production|]
    let (lower, upper, connstr1, connstr2) =
            case backendS of
                "s" -> ("sqlite", "Sqlite", "debug.db3", "production.db3")
                "p" -> ("postgresql", "Postgresql", pconn1, pconn2)
                _ -> error $ "Invalid backend: " ++ backendS


    let writeFile' fp s = do
            putStrLn $ "Generating " ++ fp
            writeFile (dir ++ '/' : fp) s
        mkDir fp = createDirectoryIfMissing True $ dir ++ '/' : fp

    mkDir "Handler"
    mkDir "hamlet"
    mkDir "cassius"
    mkDir "julius"

    writeFile' "simple-server.hs" [$codegen|
import Controller
import Network.Wai.Handler.SimpleServer (run)

main :: IO ()
main = putStrLn "Loaded" >> with~sitearg~ (run 3000)
|]

    writeFile' "fastcgi.hs" [$codegen|
import Controller
import Network.Wai.Handler.FastCGI (run)

main :: IO ()
main = with~sitearg~ run
|]

    writeFile' (project ++ ".cabal") [$codegen|
name:              ~project~
version:           0.0.0
license:           BSD3
license-file:      LICENSE
author:            ~name~
maintainer:        ~name~
synopsis:          The greatest Yesod web application ever.
description:       I'm sure you can say something clever here if you try.
category:          Web
stability:         Experimental
cabal-version:     >= 1.6
build-type:        Simple
homepage:          http://www.yesodweb.com/~project~

Flag production
    Description:   Build the production executable.
    Default:       False

executable         simple-server
    if flag(production)
        Buildable: False
    cpp-options:   -DDEBUG
    main-is:       simple-server.hs
    build-depends: base >= 4 && < 5,
                   yesod >= 0.5 && < 0.6,
                   wai-extra,
                   directory,
                   bytestring,
                   persistent,
                   persistent-sqlite,
                   template-haskell,
                   hamlet
    ghc-options:   -Wall
    extensions:    TemplateHaskell, QuasiQuotes, TypeFamilies

executable         fastcgi
    if flag(production)
        Buildable: True
    else
        Buildable: False
    main-is:       fastcgi.hs
    build-depends: wai-handler-fastcgi
    ghc-options:   -Wall
    extensions:    TemplateHaskell, QuasiQuotes, TypeFamilies
|]

    writeFile' "LICENSE" [$codegen|
The following license covers this documentation, and the source code, except
where otherwise indicated.

Copyright 2010, ~name~. All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS "AS IS" AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|]

    writeFile' (sitearg ++ ".hs") [$codegen|
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
module ~sitearg~
    ( ~sitearg~ (..)
    , ~sitearg~Route (..)
    , resources~sitearg~
    , Handler
    , module Yesod
    , module Settings
    , module Model
    ) where

import Yesod
import Yesod.Helpers.Static
import qualified Settings
import System.Directory
import qualified Data.ByteString.Lazy as L
import Yesod.WebRoutes
import Database.Persist.GenericSql
import Settings (hamletFile, cassiusFile, juliusFile)
import Model

data ~sitearg~ = ~sitearg~
    { getStatic :: Static
    , connPool :: Settings.ConnectionPool
    }

type Handler = GHandler ~sitearg~ ~sitearg~

mkYesodData "~sitearg~" [$parseRoutes|
/ RootR GET POST
/static StaticR Static getStatic
/favicon.ico FaviconR GET
/robots.txt RobotsR GET
|~~]

instance Yesod ~sitearg~ where
    approot _ = Settings.approot
    defaultLayout widget = do
        mmsg <- getMessage
        pc <- widgetToPageContent $ do
            widget
            addStyle $(Settings.cassiusFile "default-layout")
        hamletToRepHtml $(Settings.hamletFile "default-layout")
    urlRenderOverride a (StaticR s) =
        Just $ uncurry (joinPath a Settings.staticroot) $ format s
      where
        format = formatPathSegments ss
        ss :: Site StaticRoute (String -> Maybe (GHandler Static ~sitearg~ ChooseRep))
        ss = getSubSite
    urlRenderOverride _ _ = Nothing
    addStaticContent ext' _ content = do
        let fn = base64md5 content ++ '.' : ext'
        let statictmp = Settings.staticdir ++ "/tmp/"
        liftIO $ createDirectoryIfMissing True statictmp
        liftIO $ L.writeFile (statictmp ++ fn) content
        return $ Just $ Right (StaticR $ StaticRoute ["tmp", fn] [], [])

instance YesodPersist ~sitearg~ where
    type YesodDB ~sitearg~ = SqlPersist
    runDB db = fmap connPool getYesod >>= Settings.runConnectionPool db
|]

    writeFile' "Controller.hs" [$codegen|
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Controller
    ( with~sitearg~
    ) where

import ~sitearg~
import Settings
import Yesod.Helpers.Static
import Database.Persist.GenericSql

import Handler.Root

mkYesodDispatch "~sitearg~" resources~sitearg~

getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "favicon.ico"

getRobotsR :: Handler RepPlain
getRobotsR = return $ RepPlain $ toContent "User-agent: *"

with~sitearg~ :: (Application -> IO a) -> IO a
with~sitearg~ f = Settings.withConnectionPool $ \p -> do
    flip runConnectionPool p $ runMigration $ do
        migrate (undefined :: Message)
    let h = ~sitearg~ s p
    toWaiApp h >>= f
  where
    s = fileLookupDir Settings.staticdir typeByExt
|]

    writeFile' "Handler/Root.hs" [$codegen|
{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Root where

import ~sitearg~
import Control.Applicative

messageFormlet :: Formlet sub master Message
messageFormlet x = fieldsToTable
    $ Message <$> textareaField "Message"
                                (fmap messageContent x)

getRootR :: Handler RepHtml
getRootR = do
    messages <- runDB $ selectList [] [] 10 0
    (_, wform, _) <- runFormGet $ messageFormlet Nothing
    defaultLayout $ do
        setTitle "~project~ homepage"
        ident <- newIdent
        form <- extractBody wform
        addBody $(hamletFile "homepage")
        addStyle $(cassiusFile "homepage")
        addJavascript $(juliusFile "homepage")

postRootR :: Handler ()
postRootR = do
    (res, _, _) <- runFormPost $ messageFormlet Nothing
    case res of
        FormSuccess message -> runDB (insert message) >> return ()
        _ -> return ()
    redirect RedirectTemporary RootR
|]

    writeFile' "Model.hs" [$codegen|
{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving #-}
module Model where

import Yesod

mkPersist [$persist|
Message
    content Textarea
|~~]
|]

    writeFile' "Settings.hs" [$codegen|
{-# LANGUAGE CPP #-}
module Settings
    ( hamletFile
    , cassiusFile
    , juliusFile
    , connStr
    , ConnectionPool
    , withConnectionPool
    , runConnectionPool
    , approot
    , staticroot
    , staticdir
    ) where

import qualified Text.Hamlet as H
import qualified Text.Cassius as H
import qualified Text.Julius as H
import Language.Haskell.TH.Syntax
import Database.Persist.Sqlite
import Yesod (MonadCatchIO)

hamletFile :: FilePath -> Q Exp
#ifdef DEBUG
hamletFile x = H.hamletFileDebug $ "hamlet/" ++ x ++ ".hamlet"
#else
hamletFile x = H.hamletFile $ "hamlet/" ++ x ++ ".hamlet"
#endif

cassiusFile :: FilePath -> Q Exp
#ifdef DEBUG
cassiusFile x = H.cassiusFileDebug $ "cassius/" ++ x ++ ".cassius"
#else
cassiusFile x = H.cassiusFile $ "cassius/" ++ x ++ ".cassius"
#endif

juliusFile :: FilePath -> Q Exp
#ifdef DEBUG
juliusFile x = H.juliusFileDebug $ "julius/" ++ x ++ ".julius"
#else
juliusFile x = H.juliusFile $ "julius/" ++ x ++ ".julius"
#endif

connStr :: String
#ifdef DEBUG
connStr = "debug.db3"
#else
connStr = "production.db3"
#endif

connectionCount :: Int
connectionCount = 10

withConnectionPool :: MonadCatchIO m => (ConnectionPool -> m a) -> m a
withConnectionPool = withSqlitePool connStr connectionCount

runConnectionPool :: MonadCatchIO m => SqlPersist m a -> ConnectionPool -> m a
runConnectionPool = runSqlPool

approot :: String
#ifdef DEBUG
approot = "http://localhost:3000"
#else
approot = "http://localhost:3000"
#endif

staticroot :: String
staticroot = approot ++ "/static"

staticdir :: FilePath
staticdir = "static"
|]

    writeFile' "cassius/default-layout.cassius" [$codegen|
body
    font-family: sans-serif
|]

    writeFile' "hamlet/default-layout.hamlet" [$codegen|
!!!
%html
    %head
        %title $pageTitle.pc$
        ^pageHead.pc^
    %body
        $maybe mmsg msg
            #message $msg$
        ^pageBody.pc^
|]

    writeFile' "hamlet/homepage.hamlet" [$codegen|
%h1 Hello
%p#$ident$ Welcome.
%h3 Messages
$if null.messages
    %p No messages.
$else
    %ul
        $forall messages m
            %li $messageContent.snd.m$
%h3 Add Message
%form!method=post!action=@RootR@
    %table
        ^form^
        %tr
            %td!colspan=2
                %input!type=submit!value="Add Message"
|]

    writeFile' "cassius/homepage.cassius" [$codegen|
body
    font-family: sans-serif
h1
    text-align: center
|]

    writeFile' "julius/homepage.julius" [$codegen|
window.onload = function(){
    document.getElementById("%ident%").innerHTML = "<i>Added from JavaScript.</i>";
}
|]

    S.writeFile (dir ++ "/favicon.ico")
        $(runIO (S.readFile "favicon.ico") >>= \bs -> do
            pack <- [|S.pack|]
            return $ pack `AppE` LitE (StringL $ S.unpack bs))
