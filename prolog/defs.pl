:- use_module(library(lists)).

/*                 Utility functions                */
% find(+Key, +List, -Value)
find(E, [(E, R)|_], R) :- !.
find(E, [_|T], R) :-
	find(E, T, R).

% drop(+Number, +List, -NewList)
drop(0, L, L).
drop(N, [_|T], R) :-
	M is N - 1,
	drop(M, T, R).

% print_automaton(+Automaton)
% prints current state of the automaton
print_automaton(Automaton) :-
	Automaton = aut(Stack, Inputs, _),
	write("Current state: "),
	write(Stack), write(" "), write(Inputs),
	nl.

/* Get next state of the automaton for given input I
   aut contains of tuple (stack, input to be read, table) */
run(EmptyAutomaton, EmptyAutomaton) :-          % stop running if there's no input to be read  
	EmptyAutomaton = aut(_, [], _), !.         % TODO save: nice way of aliasing
                                               % Do not check other branches if all input's read.
run(Automaton, NextAutomaton) :-
	Automaton = aut(Stack, _, Table),
	print_automaton(Automaton),
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

lr(lr0, [(1, shift([('a', 3), ('b', 4), (nt('A'), 2), (nt('S'), 0)])),
		 (2, shift([('a', 3), ('b', 4), (nt('A'), 5)])),
		 (3, shift([('a', 3), ('b', 4), (nt('A'), 6)])),
         (4, reduce(nt('A'), 1)),
		 (5, reduce(nt('S'), 2)),
		 (6, reduce(nt('A'), 2))]).
