#!/bin/bash

ghc --make -prof -auto-all -caf-all -fforce-recomp -rtsopts SimpleTest.hs
time ./SimpleTest +RTS -hc -p
hp2ps -e8in -c ./SimpleTest.hp
less ./SimpleTest.prof
