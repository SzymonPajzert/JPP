#!/bin/bash

bnfc -m grammar.cf

happy -gcad --info=gram ParGrammar.y
alex -g LexGrammar.x
ghc --make TestGrammar.hs -o TestGrammar

mkdir -p res/good

fail_count=0

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
		fail_count=$((fail_count + 1))
	fi
done

echo "$fail_count tests failed"


