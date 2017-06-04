:- use_module(library(lists)).

/*                             Utility functions                             */
% find(+Key, +List, -Value)
find(E, [(E, R)|_], R) :- !.
find(E, [_|T], R) :-
	find(E, T, R).

% drop(+Number, +List, -NewList)
drop(0, L, L).
drop(N, [_|T], R) :-
	M is N - 1,
	drop(M, T, R).

% drop(+Set, +ToBeAdded, -NewSet, -NewlyAdded)
% union of two sets with new elements underlined
merge_set(Set, ToBeAdded, NewSet, NewlyAdded) :-
	union(Set, ToBeAdded, NewSet),
	subtract(NewSet, Set, NewlyAdded).


%                            Grammar functions                                %
get_productions(E, gramatyka(_, [P|_]), Result) :-
	P = prod(E, _),
	transform(P, Result), !.

get_productions(E, gramatyka(_, [_|T]),      R) :-
	get_productions(E, gramatyka(_, T), R).


% normalize the grammar E adding production Z -> E#
normalize(InputGrammar, OutputGrammar) :-
	InputGrammar = gramatyka(StartSym, Productions),

	NewProd = prod('Z', [[nt(StartSym), '#']]),
	OutputGrammar = gramatyka('Z', [NewProd|Productions]).

% TODO change to return list of productions
% transform(+prod(...), -[lrprod(...)])
transform(Production, []) :-
	Production = prod(_, []), !.

transform(Production, LRProductions) :-
	Production = prod(NonTerm, [RightProd|ProdTail]),

	transform(prod(NonTerm, ProdTail), LRProductionTail),

	LRProduction = lrprod(NonTerm, [], RightProd),

	LRProductions = [LRProduction|LRProductionTail].



closure(Grammar, Prod, Closure) :- closure_iter(Grammar, [], Prod, Closure).

% closure(+gramatyka(...), +[lrprod(...)], -[lrprod(...)])
closure_iter(_, Final, [], Final) :- !.
closure_iter(Grammar, Acc, NewProds, Closure) :-

	NewProds = [Prod|Tail],
	closure_help(Grammar, Prod, Needed),

	merge_set(Acc, [Prod], AccWithProd, _),
	merge_set(AccWithProd, Needed, NewAcc, NewNeeded),
	append(Tail, NewNeeded, Next),

	closure_iter(Grammar, NewAcc, Next, Closure).


closure_help(_, Prod, []) :-
	Prod = lrprod(_, _, []), !.      % no more needed for closure if there's nothing to be read

closure_help(Grammar, Prod, Needed) :-
	Prod = lrprod(_, _, [First|_]),
	(First = nt(NextNonTerm)
	-> (get_productions(NextNonTerm, Grammar, Needed))
	; Needed = []).          % if next is terminal, there's nothing needed

edges(EmptyGrammar, []) :-
	EmptyGrammar = gramatyka(_, []), !.

edges(Grammar, Edges) :-
	Grammar = gramatyka(_, Productions),
	Productions = [Prod|Tail],
	Prod = prod(_, RightHands),

	append(RightHands, ResultFlat),
	list_to_set(ResultFlat,

	edges(gramatyka(_, Tail), RestResult),

	union(ResultSet, RestResult, Edges).

%                            AUTOMATON FUNCTIONS							  %

% print_automaton(+Automaton)
% prints current state of the automaton
print_automaton(Automaton) :-
	Automaton = aut(Stack, Inputs, _),
	write("Current state: "),
	write(Stack), write(" "), write(Inputs),
	nl.

/* Get next state of the automaton for given input I
   aut contains of tuple (stack, input to be read, table) */
run(EmptyAutomaton, EmptyAutomaton) :-        % Stop if all input's read
	EmptyAutomaton = aut(_, [], _), !.        % TODO save: nice way of aliasing

run(Automaton, NextAutomaton) :-
	Automaton = aut(Stack, _, Table),
	Stack = [StackTop|_],
	find(StackTop, Table, Action),
	perform_action(Automaton, Action, MidAutomaton),
	run(MidAutomaton, NextAutomaton).         % Next recursive call.

perform_action(Automaton, shift(Row), NextAutomaton) :-
	Automaton = aut(S, [Input|Is], T),
	find(Input, Row, NextState),
	NextAutomaton = aut([NextState|S], Is, T).

perform_action(Automaton, reduce(Result,Number), NewAutomaton) :-
	Automaton = aut(Stack, Inputs, Table),
	drop(Number, Stack, NewStack),
	NewAutomaton = aut(NewStack, [Result|Inputs], Table).

accept(Table, Words) :-
	run(aut([1], Words, Table), aut([0|_], [], _)).