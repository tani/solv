
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

:- module(cpc, [cpc_probable/2]).
:- use_module(library(lists)).

path([imply(A, B)|C], P) :-
	path([not(A)|C], P);
	path([B|C], P).
path([not(imply(A, B))|C], P) :-
	path([A,not(B)|C], P).
path([or(A, B)|C], P) :-
	path([A|C], P);
	path([B|C], P).
path([not(or(A, B))|C], P) :-
	path([not(B),not(A)|C], P).
path([not(not(A))|B], P) :-
	path([A|B], P).
path([and(A, B)|C], P) :-
	path([A,B|C], P).
path([not(and(A, B))|C], P) :-
	path([not(A)|C], P);
	path([not(B)|C], P).
path([A|B], [A|P]) :-
	atom(A),
	path(B, P).
path([not(A)|B], [not(A)|P]) :-
	atom(A),
	path(B, P).
path([], []).

inconsistent(P) :-
	member(A, P),
	member(not(A), P).

cpc_probable(Assumptions, Conclusions) :-
	forall(member(C, Conclusions), forall(path([not(C)|Assumptions], P), inconsistent(P))).
