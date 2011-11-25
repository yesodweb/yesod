#!/bin/bash -ex

ghc --make bench/pong.hs
ghc --make bench/pong.hs -prof -osuf o_p -caf-all -auto-all -rtsopts
./bench/pong +RTS -p &
sleep 2
ab -n 1000 -c 5 http://localhost:3000/ 2>&1 | grep 'Time taken'
curl http://localhost:3000/kill
