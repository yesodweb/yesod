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
