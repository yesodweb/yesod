## yesod

The yesod package groups together the various Yesod related packages into one
cohesive whole. This is the "battery loaded" version of Yesod, whereas most of
the core code lives in
[yesod-core](http://www.stackage.org/package/yesod-core/).

For the yesod executable, see [yesod-bin](http://www.stackage.org/package/yesod-bin/).

Yesod is [fully documented on its website](http://www.yesodweb.com/).

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
