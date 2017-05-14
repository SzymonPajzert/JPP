interpreter: Main.hs gram
	make gram
	ghc --make Main.hs -o interpreter

gram: grammar.cf
	bnfc grammar.cf
	happy -ga$(HAPPY_DEBUG) ParGrammar.y
	alex -g LexGrammar.x




clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocGrammar.* LexGrammar.* ParGrammar.* LayoutGrammar.* SkelGrammar.* PrintGrammar.* TestGrammar.* AbsGrammar.* TestGrammar ErrM.* SharedString.* ComposOp.* grammar.dtd XMLGrammar.*

