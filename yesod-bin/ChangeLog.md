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
