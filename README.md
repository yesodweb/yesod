An advanced web framework using the Haskell programming language. Featuring:

  * safety & security guaranteed at compile time
  * developer productivity: tools for all your basic web development needs
  * raw performance
    * fast, compiled code
    * techniques for constant-space memory consumption
  * asynchronous IO
    * this is built in to the Haskell programming language (like Erlang)
    * handles a greater concurrent load than any other web application server

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

We recommend using [virthualenv](http://hackage.haskell.org/package/virthualenv) when hacking on Yesod.
This is optional, but prevents your custom build of Yesod from interfering with your currently installed cabal packages.

virthualenv will not work on Windows - Windows users should use only cabal-dev.

* virthualenv creates an isolated environment like cabal-dev
* virthualenv works at the shell level, so every shell must activate the virthualenv
* cabal-dev by default isolates a single cabal package, but virthualenv isolates multiple packages together.
* cabal-dev can isolate multiple packages together by using the -s sandbox argument

To just install Yesod from github, we only need cabal-dev. For hacking we prefer virthualenv: it is more convenient to just use normal cabal commands rather than `cabal-dev -s`.


### cabal-src

Michael Snoyman just released the cabal-src tool, which helps resolve dependency conflicts when installing local packages. This capability is already built in if you are using cabal-dev. Otherwise install it with:

    cabal install cabal-src

Whenever you would use `cabal install` for a local package, use `cabal-src-install` instead. Our installer script now uses cabal-src-install when it is available.


### Building Yesod

~~~ { .bash }
# update your package database if you haven't recently
cabal update
# install required libraries
cabal install Cabal cabal-install

# use cabal-dev
cabal install cabal-dev

# or use virthualenv
cabal install cabal-src virthualenv
cd yesodweb # the folder where you put the yesod, persistent, hamlet & wai repos
virthualenv --name=yesod
. .virthualenv/bin/activate

# clone and install all repos
# see below about first using virthualenv/cabal-dev before running ./scripts/install
for repo in hamlet persistent wai yesod; do
  git clone http://github.com/yesodweb/$repo
  (
    cd $repo
    git submodule update --init
    ./scripts/install
  )
done
~~~


#### installing repo packages

~~~ { .bash }
# install and test all packages in a repo
./scripts/install

# If things seem weird, you may need to do a clean.
./scripts/install --clean

# move to the individual package you are working on
cd shakespeare-text

# build and test the individual package
cabal configure -ftest --enable-tests
cabal build
cabal test
~~~


### Use your development version of Yesod in your application

Note that we have recommended to you to install Yesod into a sandboxed virthualenv environment.
This is great for development, but when you want to use these development versions in your application that means they are not available through your user/global cabal database for your application.
You should just continue to use your yesod virthualenv shell for your application.
You can also use the same`cabal-dev shared sandbox.
