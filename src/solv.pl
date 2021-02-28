#!/usr/bin/env swipl

/* 
 * This file is part of the solv distribution (https://github.com/tani/solv).
 * Copyright (c) 2021 TANIGUCHI Masaya.
 * 
 * This program is free software: you can redistribute it and/or modify  
 * it under the terms of the GNU General Public License as published by  
 * the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful, but 
 * WITHOUT ANY WARRANTY; without even the implied warranty of 
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License 
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

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
