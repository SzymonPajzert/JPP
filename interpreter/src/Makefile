all: interpreter interpret_test

interpreter: Main.hs gram
	ghc --make Main.hs -o interpreter

interpret_test: DynGrammar.hs Compile.hs TestFiles.hs gram
	ghc --make TestFiles.hs -o interpret_test

gram: grammar.cf
	bnfc grammar.cf >/dev/null
	happy -ga$(HAPPY_DEBUG) --info=GramDoc.txt ParGrammar.y >/dev/null
	alex -g LexGrammar.x >/dev/null
	ghc --make TestGrammar.hs -o TestGrammar


clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocGrammar.* LexGrammar.* ParGrammar.* LayoutGrammar.* SkelGrammar.* PrintGrammar.* TestGrammar.* AbsGrammar.* TestGrammar ErrM.* SharedString.* ComposOp.* grammar.dtd XMLGrammar.*
