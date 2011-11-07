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

If you are concerned about mucking with your installed packages, you might try using the new [virthualenv](http://hackage.haskell.org/package/virthualenv) tool to isolate a custom yesod build to a particular application.

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
