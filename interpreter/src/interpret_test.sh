#!/usr/bin/env bash
set -e

make interpret_test
cd ..
cabal build interpret_test
cd src
../dist/build/interpret_test/interpret_test
