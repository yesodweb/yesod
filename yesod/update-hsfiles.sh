#!/bin/bash -ex

rm -rf yesod-scaffold
git clone https://github.com/yesodweb/yesod-scaffold yesod-scaffold
cd yesod-scaffold

for branch in `git branch --no-color -a | grep remotes | grep -v HEAD | grep -v master`
do
    git checkout $branch
    git checkout -b ${branch##*/}
done

git checkout master
runghc build.hs
cp hsfiles/* ../hsfiles

rm -rf yesod-scaffold
