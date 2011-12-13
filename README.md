An advanced web framework using the Haskell programming language. Featuring:

  * safety & security guaranteed at compile time
  * performance
    * fast, compiled code
    * a greater concurrent load than any other web application server
  * developer productivity: tools for all your basic web development 
    needs

## Learn more: http://yesodweb.com/

## Installation: http://www.yesodweb.com/page/five-minutes

    cabal update && cabal install yesod

## Create a new project after installing

    yesod init


## Using cabal-dev

cabal-dev creates a sandboxed environment for an individual cabal package.
Your application is a cabal package and you should use cabal-dev with your Yesod application.
Instead of using the `cabal` command, use the `cabal-dev` command.

Use `yesod-devel --dev` when developing your application.

## Installing the latest development version from github

Yesod is broken up into 4 separate code repositories each built upon many smaller packages.

Install conflicts are unfortunately common in Haskell development.
However, we can prevent most of them by using some extra tools.
This will require a little up-front reading and learning, but save you from a lot of misery in the long-run.
See the above explanation of cabal-dev, and below of virthualenv.

Please note that cabal-dev will not work in a virthualenv shell - you can't use both at the same time.

### virthualenv

To just install Yesod from github, we only need cabal-dev. However, cabal-dev may be more hassle than it is worth when hacking on Yesod.

We recommend using [virthualenv](http://hackage.haskell.org/package/virthualenv) when hacking on Yesod.
This is optional, but prevents your custom build of Yesod from interfering with your currently installed cabal packages.
virthualenv creates an isolated environment like cabal-dev.
cabal-dev isolates a single cabal package, but virthualenv isolates multiple packages together.

virthualenv works at the shell level, so every shell must activate the virthualenv.

### cabal-src

Michael just released the cabal-src tool. Whenever you would use `cabal install` for a local package, use `cabal-src-install` instead.
Our installer script now uses cabal-src-install when it is available.

### Building Yesod

~~~ { .bash }
# update your package database if you haven't recently
cabal update
# install required libraries
cabal install Cabal cabal-install cabal-src virthualenv

# clone and install all repos
# see below about first using virthualenv before running ./scripts/install
for repo in hamlet persistent wai yesod; do
  git clone http://github.com/yesodweb/$repo
  (
    cd $repo
    git submodule update --init
    ./scripts/install
  )
done
~~~

### Hacking on Yesod

To prevent Yesod from conflicting with your other installs, you should use virthualenv, although it is optional.

#### virthualenv

~~~ { .bash }
cabal update
cabal install virthualenv
cd yesodweb
virthualenv --name=yesod
./virthualenv/bin/activate
~~~

#### individual cabal packages

~~~ { .bash }
# install and test all packages
./scripts/install

# move to the individual package you are working on
cd shakespeare-text

# build and test the individual package
cabal configure -ftest --enable-tests
cabal build
cabal test
~~~

#### cabal-dev

cabal-dev works very well if you are working on a single package, but it can be very cumbersome to work on multiple packages at once.

### Use your development version of Yesod in your application

Note that we have told you to install Yesod into a sandboxed virthualenv environment.
This means it is not available through your user/global cabal database for your application.
Instead you should use `cabal-dev install` to retrieve these packages.
cd to your application directory, and the reference the source list.

~~~ { .bash }
cabal-dev install /path/to/yesodweb/yesod/*(/)
~~~
