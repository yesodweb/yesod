A next generation web framework using the Haskell programming language, 
featuring:

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


## Installing the latest development version from github

Currently there is a very annoying issue that aeson, a Yesod dependency requires deepseq < 1.2
A new version of aeson should be released fairly soon.
This means that before installing Yesod you should

~~~
cabal install deepseq-1.1.0.2
~~~

To ensure that cabal prefers this package you need to first start from a clean slate.
The easiest way to do that is to use [virthualenv](http://hackage.haskell.org/package/virthualenv), which will prevent any conflicts with presently installed packages by creating an isolated install environment.

~~~
cabal install virthualenv
mkdir yesodweb
cd yesodweb
virthualenv --name=yesod
~~~

Yesod is broken up into 4 separate repos and built upon many smaller packages.
All of them can be installed with the below command.

~~~ { .bash }
cabal update

for repo in hamlet persistent wai yesod; do
  git clone http://github.com/yesodweb/$repo
  (
    cd $repo
    git submodule update --init
    ./scripts/install
  )
done
~~~
