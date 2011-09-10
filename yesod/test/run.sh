#!/bin/bash -ex
#
# A wrapper for the shelltest test. Passes along options to shelltest.
#
#   cabal install shelltestrunner

cabal clean && cabal install && cabal sdist

# I am not that good at shell scripting
# this for loop only operates on 1 file (as per tail -1)
for f in $(ls -1rt dist/*.tar.gz | tail -1)
do
  tar -xzvf $f && cd `basename $f .tar.gz`

  # shelltest is designed to show you the diff of an expected stdout/stdin. We don't care about that. If it compiles, it compiles
  # shelltest ../test/scaffold.shelltest --color --diff --all $@ -- --hide-successes

  ../test/scaffold.sh < ../test/sqlite-input.txt &&
  ../test/scaffold.sh < ../test/postgresql-input.txt &&
  ../test/scaffold.sh < ../test/tiny-input.txt &&
  ../test/scaffold.sh < ../test/mongodb-input.txt ||
    (echo "FAILED" && exit 1)
  cd ..
  rm -r `basename $f .tar.gz`
done
echo "PASSED"
