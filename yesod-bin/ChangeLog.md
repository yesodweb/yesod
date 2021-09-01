# ChangeLog for yesod-bin

## 1.6.1

Added command line options `cert` and `key` to allow TLS certificate and key files to be passed to `yesod devel` [#1717](https://github.com/yesodweb/yesod/pull/1717)

## 1.6.0.6

Fix the `add-handler` subcommand to support both the old default routes filename (`routes`) and the new one (`routes.yesodroutes`) [#1688](https://github.com/yesodweb/yesod/pull/1688)

## 1.6.0.5

* Use process groups to ensure GHC is killed on Ctrl-C [#1683](https://github.com/yesodweb/yesod/pull/1683)

## 1.6.0.4

* Support Cabal 3.0

## 1.6.0.3

* Support Cabal 2.2 [#1151](https://github.com/yesodweb/yesod/issues/1511)

## 1.6.0.2

* Fix broken support for older http-reverse-proxy

## 1.6.0.1

* Support for http-reverse-proxy 0.6

## 1.6.0

* Upgrade to conduit 1.3.0
* Remove configure, build, touch, and test commands

## 1.5.3

* Support typed-process-0.2.0.0

## 1.5.2.6

* Drop an upper bound

## 1.5.2.5

* Support for `add-handler` when modules are in `src/` directory [#1413](https://github.com/yesodweb/yesod/issues/1413)

## 1.5.2.4

* Cabal 2.0 support

## 1.5.2.3

* Fix race condition which leads dev server to stay in compilation mode. [#1380](https://github.com/yesodweb/yesod/issues/1380)

## 1.5.2.2

* I guess `--no-nix-pure` implies Nix... sigh [#1359](https://github.com/yesodweb/yesod/issues/1359)

## 1.5.2.1

* Use `--no-nix-pure` [#1357](https://github.com/yesodweb/yesod/issues/1357)

## 1.5.2

* Fix warnings

## 1.5.1

* Add `--host` option to `yesod devel`

## 1.5.0.1

* Fix build failure

## 1.5.0

Rewrite of `yesod devel` to take advantage of Stack for a simpler codebase.

Advantages:

* Does not link against the ghc library, so can be used with multiple
  GHC versions
* Leverages Stack's ability to check for dependent files, which is
  more robust than what yesod devel was doing previously
* Seems to involve less rebuilding of the library on initial run

Disadvantages:

* Lost some functionality (e.g., failure hooks, controlling the exit
  command)
* Newer codebase, quite likely has bugs that need to be ironed out.

## 1.4.18.7

* Actually release the changes for #1284

## 1.4.18.6

* Fix support for GHC 8.0.1 [#1284](https://github.com/yesodweb/yesod/issues/1284)

## 1.4.18.5

* yesod-bin: Make it build with latest optparse-applicative [#1282](https://github.com/yesodweb/yesod/pull/1282)

## 1.4.18.4

* Link yesod-bin with wxneeded on OpenBSD. [#1281](https://github.com/yesodweb/yesod/pull/1281)

## 1.4.18.3

* Adding a new handler adds it under wrong stanza [#1273](https://github.com/yesodweb/yesod/issues/1273)

## 1.4.18.2

* Work around change in behavior in newer optparse-applicative ([mailing list discussion](https://groups.google.com/d/msg/yesodweb/BrTkMKFREgU/AKVc9AK2AQAJ))

## 1.4.18.1

* error handling when checking for stack binary [#1219](https://github.com/yesodweb/yesod/pull/1219)
* GHC 8 support

## 1.4.18

* Disable `yesod test` when using Stack [#1198](https://github.com/yesodweb/yesod/issues/1198)

## 1.4.17

* Fully remove the `yesod init` command

## 1.4.16.1

* Workaround for [wai#478](https://github.com/yesodweb/wai/issues/478)

## 1.4.16

* Some updates for better reverse proxying [yesod-scaffold#114](https://github.com/yesodweb/yesod-scaffold/issues/114)

## 1.4.15

* Deprecate yesod init

## 1.4.14

* Fix order of -package-db arguments to runghc [#1057](https://github.com/yesodweb/yesod/issues/1057)

## 1.4.13

* Enable stack with yesod keter [#1041](https://github.com/yesodweb/yesod/pull/1041)

## 1.4.12

* Devel server: have to type quit to quit

## 1.4.11

* Add support to `yesod devel` to detect and use `GHC_PACKAGE_PATH`. This makes
  `yesod devel` compatible with `stack`, just run: `stack exec -- yesod devel`.

## 1.4.10

* Scaffolding update

## 1.4.9.2

* Collapse paths in keter bundles, see [mailing list thread](https://groups.google.com/d/msg/yesodweb/Ndd310qfSEc/pZOXldsKowsJ)

## 1.4.9

* Command line options for `yesod init` [#986](https://github.com/yesodweb/yesod/pull/986)

## 1.4.8

* Drop system-filepath

## 1.4.7.2

* Scaffolding updates, including fix for [#982](https://github.com/yesodweb/yesod/issues/982)

## 1.4.7

* GHC 7.10 support

## 1.4.6

* Add TLS support to `yesod devel` [#964](https://github.com/yesodweb/yesod/pull/964)

## 1.4.5

* add a switch to yesod to skip deploying a .keter with copy-to [#952](https://github.com/yesodweb/yesod/issues/952)

## 1.4.4

* Add and process Keter option 'extraFiles' [#947](https://github.com/yesodweb/yesod/pull/947)

## 1.4.3.11

* Disregard proxy environment variables in yesod devel [#945](https://github.com/yesodweb/yesod/pull/945)

## 1.4.3.10

* Allow blaze-builder 0.4

## 1.4.3.9

* Scaffold update: minimal scaffold uses yesod-core instead of yesod [yesodweb/yesod-scaffold#65](https://github.com/yesodweb/yesod-scaffold/issues/65)

## 1.4.3.8

* Scaffold update: fix 404 for missing sourcemap

## 1.4.3.6

* Scaffold update: use `addToken` instead of `addNonce`

## 1.4.3.5

* Fix add-handler putting two routes on one line [#922](https://github.com/yesodweb/yesod/pull/922)

## 1.4.3.4

Scaffolding updates:

* Improve `DevelMain` support
* Wipe out database during test runs
* Convenience `unsafeHandler` function
* Remove deprecated Chrome Frame code

## 1.4.3.3

More consistent whitespace in hamlet files in scaffolding [#50](https://github.com/yesodweb/yesod-scaffold/issues/50)

## 1.4.3.2

add-handler adds arguments too [#898](https://github.com/yesodweb/yesod/issues/898)

## 1.4.3

Add the minimal scaffolding

## 1.4.2

Scaffolding updates:

* Import.NoFoundation
* Explanation of static files in Settings.StaticFiles
* Explanation of environment variables in settings.yml

## 1.4.1.2

No args passed in keter.yml

## 1.4.1

Significant update to the scaffolding.

## 1.4.0.9

Allow devel.hs to be located in app/ or src/ subdirectories.

## 1.4.0.8

Updated postgres-fay scaffolding for yesod-fay 0.7.0

## 1.4.0.7

Fix a bug in `yesod devel` when cabal config has `tests: True` #864
