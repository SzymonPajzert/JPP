#!/bin/bash

# files good/*new have outputed their contents

bnfc -m grammar.cf

happy -gcad --info=gram ParGrammar.y
alex -g LexGrammar.x
ghc --make TestGrammar.hs -o TestGrammar

rm -r res
mkdir -p res/good
mkdir -p res/bad

fail_count=0

for file in good/* ; do
    echo -n "testing $file:   "
	if [[ `printf $file | tail -c 3` == "new" ]]; then
		echo ""
		cat $file
 		echo ""
	fi
	
	./TestGrammar < $file &> res/$file
	if (( $? == 0 )); then
		echo -e "OK"
	else
		echo -e "BAD"
		fail_count=$((fail_count + 1))
	fi
done

for file in bad/* ; do
    echo -n "testing $file:   "
	if [[ `printf $file | tail -c 3` == "new" ]]; then
		echo ""
		cat $file
 		echo ""
	fi
	
	./TestGrammar < $file &> res/$file
	if (( $? != 0 )); then
		echo -e "OK"
	else
		echo -e "BAD"
		fail_count=$((fail_count + 1))
	fi
done

echo -e "\n\n\n$fail_count tests failed"


