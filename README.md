A next genteration web framework using the Haskell programming language, featuring:
  * safety & security: guaranteed at compile time
  * performance: a greater concurrenct load then any other web application server
  * developer productivity: efficiently handles all your basic web development needs

## Learn more: http://yesodweb.com/


## Installation: http://www.yesodweb.com/page/five-minutes


## Create a new project after installing

    yesod init


## Installing the latest development version from github

Yesod is built upon many smaller packages, all of which can be installed with:

    cabal update

    REPOS="hamlet persistent wai yesod"
    for repo in $REPOS
    do
      git clone http://github.com/yesodweb/$repo
      (cd $repo && ./install-all.sh)
    done
