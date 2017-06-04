:- [defs].

lr(lr0, [(1, shift([('a', 3), ('b', 4), (nt('A'), 2), (nt('S'), 0)])),
		 (2, shift([('a', 3), ('b', 4), (nt('A'), 5)])),
		 (3, shift([('a', 3), ('b', 4), (nt('A'), 6)])),
         (4, reduce(nt('A'), 1)),
		 (5, reduce(nt('S'), 2)),
		 (6, reduce(nt('A'), 2))]).

gramms(gram1, gramatyka('E', [
							  prod('E', [[nt('E'),'+',nt('T')], [nt('T')]]),
							  prod('T', [[id], ['(', nt('E'), ')']])])).

gramms(gram0, gramatyka('E', [prod('E', [[nt('E'),'+',nt('E')]])])).

checkWord(S, Automat) :-
	format(" Slowo: ~p ", [S]),
	(accept(Automat, S) -> true; write('NIE ')),
	write('nalezy.\n').

checkClosure(Grammar, Prods, ExpectedResult) :-
	closure(Grammar, Prods, Result),
	permutation(Result, ExpectedResult).

?- lr(lr0, Automat),
   checkWord(['a', 'a', 'b', 'b'], Automat),
   checkWord(['a', 'a'], Automat).

?- gramms(gram1, X),
   normalize(X, Y),
   checkClosure(Y,
				[lrprod('Z', [], [nt('E'), '#'])],
				[
				 lrprod('Z', [], [nt('E'), #]),
				 lrprod('E', [], [nt('E'), +, nt('T')]),
				 lrprod('E', [], [nt('T')]),
				 lrprod('T', [], [id]),
				 lrprod('T', [], ['(', nt('E'), ')'])]).