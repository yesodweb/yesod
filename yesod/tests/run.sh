#!/bin/bash -x
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
  shelltest ../tests/scaffold.shelltest --color --diff $@ -- --hide-successes
  cd ..
  rm -r `basename $f .tar.gz`
done
