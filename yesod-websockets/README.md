
## REQUIRED PACKAGES

The example, chat.hs, requires `stm-lifted` and `conduit-combinators`
which are not dependencies of `yesod-websockets`. Installing the extra
packages needed for the chat.hs example can be installed with the
command:

    $ cabal install stm-lifted conduit-combinators


## TIMEOUTS

Versions of `yesod-websockets` prior to 0.2.1 may experience session
timeouts after about 30 seconds of inactivity. Refer to the patch in
the `websockets` library which addresses this issue:

https://github.com/jaspervdj/websockets/commit/536849d1f3265076f61edefd5c89e84e82a99c71
