#!/bin/bash

bnfc -m grammar.cf

happy -gcad --info=gram ParGrammar.y
alex -g LexGrammar.x
ghc --make TestGrammar.hs -o TestGrammar

mkdir -p res/good

for file in good/* ; do
    echo "testing $file"
	cat $file
	echo ""
	./TestGrammar < $file &> res/$file
	if (( $? == 0 )); then
		echo "OK"
		echo ""
	else
		echo "BAD"
		echo ""
	fi
done
