![Tests](https://github.com/yesodweb/yesod/workflows/Tests/badge.svg)

# Yesod Web Framework

An advanced web framework using the Haskell programming language. Featuring:

  * safety & security guaranteed at compile time
  * developer productivity: tools for all your basic web development needs
  * raw performance
    * fast, compiled code
    * techniques for constant-space memory consumption
  * asynchronous IO
    * this is built in to the Haskell programming language (like Erlang)

## Getting Started

Learn more about Yesod on [its main website](http://www.yesodweb.com/). If you
want to get started using Yesod, we strongly recommend the [quick start
guide](http://www.yesodweb.com/page/quickstart), based on [the Haskell build
tool stack](https://github.com/commercialhaskell/stack#readme).

Here's a minimal example!

```haskell
{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies #-}

import Yesod

data App = App -- Put your config, database connection pool, etc. in here.

-- Derive routes and instances for App.
mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App -- Methods in here can be overridden as needed.

-- The handler for the GET request at /, corresponds to HomeR.
getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello World!|]

main :: IO ()
main = warp 3000 App
```

To read about each of the concepts in use above (routing, handlers,
linking, JSON), in detail, visit
[Basics in the Yesod book](https://www.yesodweb.com/book/basics#basics_routing).

## Hacking on Yesod

Yesod consists mostly of four repositories:

```bash
git clone --recurse-submodules http://github.com/yesodweb/shakespeare
git clone --recurse-submodules http://github.com/yesodweb/persistent
git clone --recurse-submodules http://github.com/yesodweb/wai
git clone --recurse-submodules http://github.com/yesodweb/yesod
```

Each repository can be built with `stack build`.
