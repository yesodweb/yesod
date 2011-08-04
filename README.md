## Learn more: http://yesodweb.com/

## Installation: http://www.yesodweb.com/page/five-minutes

## Create a new project after installing

    yesod init

## Installing the latest development version from github

REPOS="hamlet persistent wai yesod"
for repo in $REPOS
do
  git clone http://github.com/yesodweb/$repo
  cd $repo && (./install-all.sh; cd ..)
done
