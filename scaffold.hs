{-# LANGUAGE QuasiQuotes #-}
import CodeGen
import System.IO
import System.Directory

main :: IO ()
main = do
    putStr [$codegen|Welcome to the Yesod scaffolder.
I'm going to be creating a skeleton Yesod project for you.
Please make sure you are in the directory where you'd like the files created.

What is your name? We're going to put this in the cabal and LICENSE files.

Your name: |]
    hFlush stdout
    name <- getLine

    putStr [$codegen|
Welcome ~name~.
What do you want to call your project? We'll use this for the cabal name and
executable filenames.

Project name: |]
    hFlush stdout
    project <- getLine
    putStr [$codegen|
Great, we'll be creating ~project~ today. What's going to be the name of
your site argument datatype? This name must start with a capital letter;
I recommend picking something short, as this name gets typed a lot.

Site argument: |]
    hFlush stdout
    sitearg <- getLine
    putStr [$codegen|
That's it! I'm creating your files now...
|]

    putStrLn $ "Generating " ++ project ++ ".cabal"
    writeFile (project ++ ".cabal") [$codegen|
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

executable         ~project~
    build-depends: base >= 4 && < 5,
                   yesod >= 0.4.0 && < 0.5.0,
                   persistent-sqlite >= 0.1.0 && < 0.2
    ghc-options:   -Wall
    main-is:       ~project~.hs
|]

    putStrLn "Generating LICENSE"
    writeFile "LICENSE" [$codegen|
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

    putStrLn ("Generating " ++ project ++ ".hs")
    writeFile (project ++ ".hs") [$codegen|
import Yesod
import App

main :: IO ()
main = with~sitearg~ $ basicHandler 3000
|]

    putStrLn "Generating App.hs"
    writeFile "App.hs" [$codegen|
{-# LANGUAGE TypeFamilies, QuasiQuotes, OverloadedStrings #-}
module App
    ( ~sitearg~ (..)
    , with~sitearg~
    ) where
import Yesod
import Yesod.Helpers.Crud
import Yesod.Helpers.Static
import Database.Persist.Sqlite
import Model

data ~sitearg~ = ~sitearg~
    { connPool :: Pool Connection
    , static :: Static
    }

with~sitearg~ :: (~sitearg~ -> IO a) -> IO a
with~sitearg~ f = withSqlite "~project~.db3" 8 $ \pool -> do
    flip runSqlite pool $ do
        -- This is where you can initialize your database.
        initialize (undefined :: Person)
    f $ ~sitearg~ pool $ fileLookupDir "static" typeByExt

type PersonCrud = Crud ~sitearg~ Person

mkYesod "~sitearg~" [$parseRoutes|
/       RootR   GET
/people PeopleR PersonCrud defaultCrud
/static StaticR Static static
|~~]

instance Yesod ~sitearg~ where
    approot _ = "http://localhost:3000"
    defaultLayout (PageContent title head' body) = hamletToContent [$hamlet|
!!!
%html
    %head
        %title $title$
        %link!rel=stylesheet!href=@stylesheet@
        ^head'^
    %body
        #wrapper
            ^body^
|~~]
      where
        stylesheet = StaticR $ StaticRoute ["style.css"]

instance YesodPersist ~sitearg~ where
    type YesodDB ~sitearg~ = SqliteReader
    runDB db = fmap connPool getYesod >>= runSqlite db

getRootR :: Handler ~sitearg~ RepHtml
getRootR = applyLayoutW $ do
    setTitle "Welcome to the ~project~ project"
    addBody [$hamlet|
%h1 Welcome to ~project~
%h2 The greatest Yesod web application ever!
%p
    %a!href=@PeopleR.CrudListR@ Manage people
|~~]
|]

    putStrLn "Generating Model.hs"
    writeFile "Model.hs" [$codegen|
{-# LANGUAGE GeneralizedNewtypeDeriving, QuasiQuotes, TypeFamilies #-}

-- We don't explicitly state our export list, since there are funny things
-- that happen with type family constructors.
module Model where

import Yesod
import Yesod.Helpers.Crud

share2 mkPersist mkToForm [$persist|
Person
    name String
    age Int
|~~]

instance Item Person where
    itemTitle = personName
|]

    putStrLn "Generating static/style.css"
    createDirectoryIfMissing True "static"
    writeFile "static/style.css" [$codegen|
body {
    font-family: sans-serif;
    background: #eee;
}

#wrapper {
    width: 760px;
    margin: 1em auto;
    border: 2px solid #000;
    padding: 0.5em;
    background: #fff;
}
|]
