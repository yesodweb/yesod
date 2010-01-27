#!/bin/sh

for f in examples/*.*hs
do
    ghc --make -Wall -Werror $f || exit
done
