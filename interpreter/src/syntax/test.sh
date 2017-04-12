#!/bin/bash
set -e

bnfc -m grammar.cf

happy -gcad --info=gram ParGrammar.y
alex -g LexGrammar.x
ghc --make TestGrammar.hs -o TestGrammar

for file in good/* ; do	
	./TestGrammar < $file
	if [[ $? != 0 ]]; then
		cat temp
	fi
done
