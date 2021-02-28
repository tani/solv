#!/usr/bin/env swipl

:- use_module('ipc.pl').
:- use_module('cpc.pl').
:- initialization(main, main).

main([help|_]) :- !,
	writeln('solv is a CPC/IPC theorem prover based on tableaux.'),
	nl,
	writeln('solv <system> <conclusion> [<assumption> ...]'),
	writeln('    <system>     ::= ipc | cpc'),
	writeln('    <conclusion> ::= <term>'),
	writeln('    <assumption> ::= <term>'),
	writeln('    <term>       ::= imply(<term>,<term>)'),
	writeln('                   | or(<term>,<term>)'),
	writeln('                   | and(<term>,<term>)'),
	writeln('                   | not(<term>)'),
	writeln('                   | <variable>'),
	writeln('    <variable>   ::= a | b | ...'),
	halt(0).

main([ipc|Argv]) :-
	maplist(term_to_atom, [Conclusion|Assumptions], Argv),
	ipc_probable(Assumptions, [Conclusion])
	 -> (write(probable), halt(0))
          ; (write(unprobable), halt(1)).

main([cpc|Argv]) :-
	maplist(term_to_atom, [Conclusion|Assumptions], Argv),
	cpc_probable(Assumptions, [Conclusion])
	 -> (write(probable), halt(0))
          ; (write(unprobable), halt(1)).
