# Yesod

An advanced web framework using the Haskell programming language. Featuring:

  * safety & security guaranteed at compile time
  * developer productivity: tools for all your basic web development needs
  * raw performance
    * fast, compiled code
    * techniques for constant-space memory consumption
  * asynchronous IO
    * this is built in to the Haskell programming language (like Erlang)
    * handles a greater concurrent load than any other web application server

# Learn more: http://yesodweb.com/

## Install the latests stable Yesod: http://www.yesodweb.com/page/quickstart

    cabal update && cabal install yesod

### Create a new project after installing

    yesod init

Your application is a cabal package and you use `cabal` to install its dependencies.

# Installing & isolation

Install conflicts are unfortunately common in Haskell development.
If you are not using any sandbox tools, you may discover that some of the other haskell installs on your system are broken.
You can prevent this by using sandbox tools: `cabal-dev` or `virthualenv`, now being renamed to `hsenv`.

Isolating an entire project with a virtual machine is also a great idea, you just need some tools to help that process.
[Vagrant](http://vagrantup.com) is a great tool for that and there is a [Haskell Platform installer](https://bitbucket.org/puffnfresh/vagrant-haskell-heroku) for it.

## Using cabal-dev

cabal-dev creates a sandboxed environment for an individual cabal package.
Instead of using the `cabal` command, use the `cabal-dev` command which will use the sandbox.

Use `yesod-devel --dev` when developing your application.



## Installing the latest development version from github for use with your application

    cabal update
    cabal install cabal-meta cabal-src

In your application folder, create a `sources.txt` file with the following contents:

    ./
    https://github.com/yesodweb/yesod
    https://github.com/yesodweb/shakespeare
    https://github.com/yesodweb/persistent
    https://github.com/yesodweb/wai

`./` means build your app. The yesod repos will be cloned and placed in a `vendor` repo.
Now run: `cabal-meta install`. If you use `cabal-dev`, run `cabal-meta --dev install`

This should work almost all of the time. You can read more on [cabal-meta](https://github.com/yesodweb/cabal-meta)
If you aren't building from an application, remove the `./` and create a new directory for your sources.txt first.



## virthualenv

We recommend using [virthualenv](http://hackage.haskell.org/package/virthualenv)/[hsenv](https://github.com/Paczesiowa/hsenv) when hacking on Yesod from Linux. This is optional, but prevents your custom build of Yesod from interfering with your currently installed cabal packages.

virthualenv will not work on Windows and maybe not Mac. Use cabal-dev instead

* virthualenv creates an isolated environment like cabal-dev
* virthualenv works at the shell level, so every shell must activate the virthualenv
* cabal-dev by default isolates a single cabal package, but virthualenv isolates multiple packages together.
* cabal-dev can isolate multiple packages together by using the -s sandbox argument


## cabal-src

The cabal-src tool helps resolve dependency conflicts when installing local packages.
This capability is already built in if you are using cabal-dev or cabal-meta. Otherwise install cabal-src with:

    cabal install cabal-src

Whenever you would use `cabal install` to install a local package, use `cabal-src-install` instead.
Our installer script now uses cabal-src-install when it is available.


## Cloning the repos

The above instructions for building the latest should work well.
But you can clone the repos without the help of cabal-meta:

~~~ { .bash }
for repo in shakespeare persistent wai yesod; do
  git clone http://github.com/yesodweb/$repo
  (
    cd $repo
    git submodule update --init
  )
done
~~~~

## Building your changes to Yesod

Yesod is composed of 4 "mega-repos", each with multiple cabal packages. `./script/install` will run tests against each package and install each package.

### install package in all repos

~~~ { .bash }
for repo in shakespeare persistent wai yesod; do
    pushd $repo
    ./scripts/install
    popd
done
~~~

### Clean build (sometimes necessary)

~~~ { .bash }
./scripts/install --clean
~~~

### Building individual packages

~~~ { .bash }
# move to the individual package you are working on
cd shakespeare-text

# build and test the individual package
cabal configure -ftest --enable-tests
cabal build
cabal test
~~~
