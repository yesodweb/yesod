# Yesod

An advanced web framework using the Haskell programming language. Featuring:

  * safety & security guaranteed at compile time
  * developer productivity: tools for all your basic web development needs
  * raw performance
    * fast, compiled code
    * techniques for constant-space memory consumption
  * asynchronous IO
    * this is built in to the Haskell programming language (like Erlang)

# Learn more: http://yesodweb.com/

## Install the latests stable Yesod: http://www.yesodweb.com/page/quickstart

    cabal update && cabal install yesod

### Create a new project after installing

    yesod init

Your application is a cabal package and you use `cabal` to install its dependencies.

# Installing & isolation

Install conflicts are unfortunately common in Haskell development.
If you are not using any sandbox tools, you may discover that some of the other haskell installs on your system are broken.
You can prevent this by using cabal sandbox.

Isolating an entire project is also a great idea, you just need some tools to help that process.
On Linux you can use Docker.
On any OS you can use a virtual machine. [Vagrant](http://vagrantup.com) is a great tool for that and there is a [Haskell Platform installer](https://bitbucket.org/puffnfresh/vagrant-haskell-heroku) for it.

## Using cabal sandbox

To sandbox a project, type:

    cabal sandbox init

This ensures that future installs will be local to the sandboxed directory.


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
Now run: `cabal-meta install`.

This should work almost all of the time. You can read more on [cabal-meta](https://github.com/yesodweb/cabal-meta)
If you aren't building from an application, remove the `./` and create a new directory for your sources.txt first.



## hsenv (Linux and Mac OS X)

[hsenv](https://github.com/tmhedberg/hsenv) also provides a sandbox, but works at the shell level.
Generally we recommend using cabal sandbox, but hsenv has tools for allowing you to use different versions of GHC, which may be useful for you.



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

The traditional Yesod stack requires 4 "mega-repos", each with multiple cabal packages. `cabal-meta install` will install each package.

### install package in all repos

~~~ { .bash }
for repo in shakespeare persistent wai yesod; do
    pushd $repo
    cabal-meta install
    popd
done
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
