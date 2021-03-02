% -*- mode: prolog -*-

% 
% This file is part of the solv distribution (https://github.com/tani/solv).
% Copyright (c) 2021 TANIGUCHI Masaya.
% 
% This program is free software: you can redistribute it and/or modify  
% it under the terms of the GNU General Public License as published by  
% the Free Software Foundation, version 3.
% 
% This program is distributed in the hope that it will be useful, but 
% WITHOUT ANY WARRANTY; without even the implied warranty of 
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
% General Public License for more details.
% 
% You should have received a copy of the GNU General Public License 
% along with this program. If not, see <http://www.gnu.org/licenses/>.
% 

:- module(ipc, [probable/4]).
:- use_module(library(lists)).

shuffle(X, Y) :-
    append([L, [M], R], X),
    append([[M], L, R], Y).
shuffle([], []).

simple(X, X).

truthy(truthy(_)).
truthy(X, truthy(X)).

falsy(falsy(_)).
falsy(X, falsy(X)).

tree([truthy(A)|_], [node(close, [])], P, _) :-
    member(falsy(A), P), !.

tree([falsy(A)|_], [node(close, [])], P, _) :-
    member(truthy(A), P), !.

tree([truthy(not(A))|X], [node([falsy(A)], C)], P, S) :-
    call(S, [falsy(A)|X], Y),
    tree(Y, C, [truthy(not(A))|P], S).

tree([falsy(not(A))|X], [node([truthy(A), exclude_falsy], C)], P, S) :-
    include(truthy, X, Y),
    include(truthy, P, Q),
    call(S, [truthy(A)|Y], Z),
    tree(Z, C, Q, S).

tree([truthy(and(A, B))|X], [node([truthy(A), truthy(B)], C)], P, S) :-
    call(S, [truthy(A), truthy(B)|X], Y),
    tree(Y, C, [truthy(and(A, B))|P], S).

tree([falsy(and(A, B))|X], [node([falsy(A)], L), node([falsy(B)], R)], P, S) :-
    call(S, [falsy(A)|X], Y1),
    call(S, [falsy(B)|X], Y2),
    tree(Y1, L, [falsy(and(A, B))|P], S),
    tree(Y2, R, [falsy(and(A, B))|P], S).

tree([truthy(or(A, B))|X], [node([truthy(A)], L), node([truthy(B)], R)], P, S) :-
    call(S, [truthy(A)|X], Y1),
    call(S, [truthy(B)|X], Y2),
    tree(Y1, L, [truthy(or(A, B))|P], S),
    tree(Y2, R, [truthy(or(A, B))|P], S).

tree([falsy(or(A, B))|X], [node([falsy(A), falsy(B)], C)], P, S) :-
    call(S, [falsy(A), falsy(B)|X], Y),
    tree(Y, C, [falsy(or(A, B))|P], S).

tree([truthy(imply(A, B))|X], [node([falsy(A)], L), node([truthy(B)], R)], P, S) :-
    call(S, [falsy(A)|X], Y1),
    call(S, [truthy(B)|X], Y2),
    tree(Y1, L, [truthy(imply(A, B))|P], S),
    tree(Y2, R, [truthy(imply(A, B))|P], S).

tree([falsy(imply(A, B))|X], [node([truthy(A), falsy(B), exclude_falsy], C)], P, S) :-
    include(truthy, X, Y),
    include(truthy, P, Q),
    call(S, [truthy(A), falsy(B)|Y], Z),
    tree(Z, C, Q, S).

tree([truthy(A)|X], C, P, S) :-
    atom(A),
    tree(X, C, [truthy(A)|P], S).

tree([falsy(A)|X], C, P, S) :-
    atom(A),
    tree(X, C, [falsy(A)|P], S).

tree([], [node(open, [])], _, _).

closed(node(close, [])) :- !, true.

closed(node(open, [])) :- !, fail.

closed(node(_, X)) :-
    forall(member(Y, X), closed(Y)).

probable(A, C, S, R) :-
    maplist(truthy, A, X),
    ((tree([falsy(C)|X], B, [], S), T=node([falsy(C)|X], B), closed(T)) -> R = probable(T);
     (tree([falsy(C)|X], B, [], S), T=node([falsy(C)|X], B), \+ closed(T)) -> R = unprobable(T)).
