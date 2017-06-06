:- use_module(library(lists)).

/*

  Reprezentacja automatu

  Automat jest reprezentowany za pomocą aut(StanPoczątkowy, TablicaPrzejść, Cel)

  TablicaPrzejść jest postaci listy par (Stan, Akcja), gdzie Akcja może być albo
  shift(...) opakowujące listę par możliwych wejść (terminal, nieterminal)
  i kolejnego stanu do przejścia. reduce(nt(Nieterminal), Liczba), mówi ile
  powinniśmy elementów zredukować ze stosu i jakim symbolem to zastąpić. Symbol
  ten jest dodawany na początek niewczytanego inputu.

  Stan jest zbiorem produkcji typu LRProd = lrprod(Nieterminal, Przeczytane,
  Nieprzeczytane). Mogliśmy tutaj optymalizować i każdy stan zastąpić pewnym
  unikalnym identyfikatorem, ale Prolog i tak nie jest demonem szybkości,
  a to jedynie zwiększa nam stałą rozwiązania.

  Automat jest uruchamiany za pomocą run, które wczytuje input póki jest.

*/  

%                              Types definitions                               %

/*

type Grammar = gramatyka(Char, [Production])
type Production = prod(Char, [RightSide])
type RightSide = [Element]
type Element = nt(Char) | Char

type LRProd = lrprod(Char, [Element], [Element])
type State = [LRProd]
  
type Graph = graph([State], [Edge])
type Edge = edge(State, Element, State)
  
type Set = List
  
*/


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
	Prod = lrprod(_, _, []), !.      % no closure if nothing is to be read 

closure_help(Grammar, Prod, Needed) :-
	Prod = lrprod(_, _, [First|_]),
	(First = nt(NextNonTerm)
	-> (get_productions(NextNonTerm, Grammar, Needed))
	; Needed = []).          % if next is terminal, there's nothing needed

%                              GRAPH FUNCTIONS                                 %

edges(EmptyGrammar, ['#']) :-
	EmptyGrammar = gramatyka(_, []), !.

edges(Grammar, Edges) :-
	Grammar = gramatyka(_, Productions),
	Productions = [Prod|Tail],
	Prod = prod(_, RightHands),

	append(RightHands, ResultFlat),
	list_to_set(ResultFlat, ResultSet),

	edges(gramatyka(_, Tail), RestResult),

	union(ResultSet, RestResult, Edges).

next_state([], _, []) :- !.
next_state(State, Input, NextState) :-
	State = [Head|Tail],
	next_state(Tail, Input, NewTail),
	
	(Head = lrprod(NonTerm, Read, Unread), Unread = [Input|NewUnread]
	-> (
		append(Read, [Input], NewRead),
		NewHead = lrprod(NonTerm, NewRead, NewUnread),
		NextState = [NewHead|NewTail])
	; NextState = NewTail).

get_root(Grammar, Root) :- get_productions('Z', Grammar, Root).

% get_graph :: (Grammar, Graph)
get_graph(Grammar, Graph) :-
	normalize(Grammar, NormalizedGrammar),
	get_root(NormalizedGrammar, Root),
	closure(Grammar, Root, ClosRoot),

	StartGraph = graph([ClosRoot], []),
	get_graph_iter(NormalizedGrammar, StartGraph, [ClosRoot], Graph).

% type NewStates = [State]
% get_graph_iter :: (Grammar, Graph, NewStates, Graph)
get_graph_iter(_, Graph, [], Graph) :- !.

get_graph_iter(Grammar, Graph, AwaitingStates, Result) :-
	AwaitingStates = [State|StatesTail],
	State = [],           % If state is empty, skip its result
	get_graph_iter(Grammar, Graph, StatesTail, Result), !.

get_graph_iter(Grammar, Graph, AwaitingStates, Result) :-
	AwaitingStates = [State|StatesTail],
	get_all_neighbours(Grammar, State, NewEdge, NewStates),
	NewEdgePacked = (State, NewEdge),
	
	
	Graph = graph(GraphStates, GraphEdges),
	union([State], GraphStates, NewGraphStates),
	NewGraphEdges = [NewEdgePacked|GraphEdges],
	NewGraph = graph(NewGraphStates, NewGraphEdges),

	union(StatesTail, NewStates, NewAwaitingStates),
	subtract(NewAwaitingStates, NewGraphStates, NewAwaitingStatesDiff),
	get_graph_iter(Grammar, NewGraph, NewAwaitingStatesDiff, Result).


% get_all_neighbours :: (Grammar, State, [Edge], [State])
% Return all neighbours of given state, for given grammar

is_reduction_state(State, Number, NonTerminal) :-
	State = [LRProduction],
	LRProduction = lrprod(NonTerminal, Read, []),
	length(Read, Number).

get_all_neighbours(_, State, NewEdge, []) :-
	is_reduction_state(State, Number, NonTerminal),
	!,                     % Do not check other branches if this fails
	NewEdge = reduce(nt(NonTerminal), Number).


get_all_neighbours(Grammar, State, NewEdgePacked, NewStates) :-
	edges(Grammar, AllLetters),
	get_all_neighbours_help(Grammar, State, AllLetters, NewEdges, NewStates),
	NewEdgePacked = shift(NewEdges).

get_all_neighbours_help(_, _, [], [], []) :- !.	
get_all_neighbours_help(Grammar, State, LettersToGo, NewEdges, NewStates) :-
	LettersToGo = [Letter|Letters],

	get_all_neighbours_help(
		Grammar, State, Letters, NewEdgesRest, NewStatesRest),

	next_state(State, Letter, NextState),
	closure(Grammar, NextState, NextStateClosure),
	NewEdge = (Letter, NextStateClosure),

	NewEdges=[NewEdge|NewEdgesRest],
	union([NextStateClosure], NewStatesRest, NewStates).

print_edge(Edge) :- write(Edge).

print_graph(Graph) :-
	Graph = graph(_, []), !.

print_graph(Graph) :-
	Graph = graph(_, Edges),
	Edges = [Edge|Tail],
	print_edge(Edge), nl,
	NewGraph = graph(_, Tail),
	print_graph(NewGraph).

%                          CHECKING GRAPH IS LR 0                              %
check_graph(EmptyGraph, Info) :-
	EmptyGraph = graph([], _),
	Info = yes, !.		% we accept graph

check_graph(EmptyGraph, Info) :-
	EmptyGraph = graph([Node|Nodes], _),
	check_node(Node, NodeInfo),

	(NodeInfo = yes
	-> check_graph(graph(Nodes, _), Info)
	 ; Info = NodeInfo).

check_node(Node, Info) :-
	only_shifts(Node),
	Info = yes, !.

check_node(Node, Info) :-
	only_reductions(Node),
	reductions_values(Node, Values),

	( Values = [_]
	-> Info = yes
	; (
	   Values = [First|[Second|_]],
	   Info = konflikt(redukcji(First,Second))
	  )
	), !.

check_node(Node, Info) :-
	get_shift_and_reduce(Node, Shift, Reduce),
	Info = konflikt(shift_reduce(Shift, Reduce)).

get_shift_and_reduce([], null, null).
get_shift_and_reduce([Shift|Tail], Shift, Reduce) :-
	is_shift(Shift),
	get_shift_and_reduce(Tail, _, Reduce), !.
	
get_shift_and_reduce([Reduce|Tail], Shift, Reduce) :-
	is_reduc(Reduce),
	get_shift_and_reduce(Tail, Shift, _), !.
	
is_shift(lrprod(_, _, [_|_])).
is_reduc(lrprod(_, _, [])).
	
only_shifts([]).
only_shifts([Head|Tail]) :-
	is_shift(Head),
	only_shifts(Tail).

only_reductions([]).
only_reductions([Head|Tail]) :-
	is_reduc(Head),
	only_reductions(Tail).

reductions_values([], []).
reductions_values([Head|Tail], Result) :-
	reductions_values(Tail, SubResult),
	Head = lrprod(NonTerminal, _, _),
	union([NonTerminal], SubResult, Result).
	
%                            AUTOMATON FUNCTIONS							   %

% print_automaton(+Automaton)
% prints current state of the automaton
print_automaton(Automaton) :-
	Automaton = aut(Stack, Inputs, _),
	write("Current state: "),
	write(Stack), write("\n"), write(Inputs),
	nl, nl.

/* Get next state of the automaton for given input I
   aut contains of tuple (stack, input to be read, table) */
run(Automaton, NextAutomaton) :-
	(Automaton = aut(_, [], _)
	-> NextAutomaton = Automaton                 % stop if input is read
	 ;
	(Automaton = aut(Stack, _, Table),
	Stack = [StackTop|_],
	find(StackTop, Table, Action),
	perform_action(Automaton, Action, MidAutomaton),
	!, 
	run(MidAutomaton, NextAutomaton))).         % Next recursive call.

perform_action(Automaton, shift(Row), NextAutomaton) :-
	Automaton = aut(S, [Input|Is], T),
	find(Input, Row, NextState),
	NextAutomaton = aut([NextState|S], Is, T).

perform_action(Automaton, reduce(Result,Number), NewAutomaton) :-
	Automaton = aut(Stack, Inputs, Table),
	drop(Number, Stack, NewStack),
	NewAutomaton = aut(NewStack, [Result|Inputs], Table).

%                                 TOP LEVEL                                    %
createLR(Gramatyka, Automat, Info) :-
	get_graph(Gramatyka, Graph),
	check_graph(Graph, Info),
	Graph = graph(_, Table),

	normalize(Gramatyka, NormGramatyka),
	get_root(NormGramatyka, Root),
	closure(Gramatyka, Root, ClosRoot),

	Gramatyka = gramatyka(MainNonterm, _),
	Goal = [lrprod('Z', [nt(MainNonterm), '#'], [])],
	
	Automat = tab(ClosRoot, Table, Goal), !.

accept(Automat, Words) :-
	append(Words, ['#'], WordsAppend),
	Automat = tab(FirstState, Table, Goal),
	run(aut([FirstState], WordsAppend, Table), Result),
	!,     % Do not backtrace if the word is not part of the language
	Result = aut([Goal|_], [], _).
