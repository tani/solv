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

:- module(ipc, [probable/3]).
:- use_module(library(lists)).

shuffle(X, Y) :-
    append([L, [M], R], X),
    append([[M], L, R], Y).
shuffle([], []).

simple(X, X).

truthy(truthy(_)).
truthy(X, truthy(X)).

falsy(truthy(_)).
falsy(X, falsy(X)).

tree([truthy(A)|_], node(truthy(A), [close]), P, _) :-
    member(falsy(A), P), !.

tree([falsy(A)|_], node(falsy(A), [close]), P, _) :-
    member(truthy(A), P), !.

tree([truthy(not(A))|X], node(truthy(not(A)), [M]), P, S) :-
    call(S, [falsy(A)|X], Y),
    tree(Y, M, [truthy(not(A))|P], S).

tree([falsy(not(A))|X], node(falsy(not(A)), [M]), P, S) :-
    include(truthy, X, Y),
    include(truthy, P, Q),
    call(S, [truthy(A)|Y], Z),
    tree(Z, M, Q, S).

tree([truthy(and(A, B))|X], node(truthy(and(A, B)), [M]), P, S) :-
    call(S, [truthy(A), truthy(B)|X], Y),
    tree(Y, M, [truthy(and(A, B))|P], S).

tree([falsy(and(A, B))|X], node(falsy(and(A, B)), [L, R]), P, S) :-
    call(S, [falsy(A)|X], Y1),
    call(S, [falsy(B)|X], Y2),
    tree(Y1, L, [falsy(and(A, B))|P], S),
    tree(Y2, R, [falsy(and(A, B))|P], S).

tree([truthy(or(A, B))|X], node(truthy(or(A, B)), [L, R]), P, S) :-
    call(S, [truthy(A)|X], Y1),
    call(S, [truthy(B)|X], Y2),
    tree(Y1, L, [truthy(or(A, B))|P], S),
    tree(Y2, R, [truthy(or(A, B))|P], S).

tree([falsy(or(A, B))|X], node(falsy(or(A, B)), [M]), P, S) :-
    call(S, [falsy(A), falsy(B)|X], Y),
    tree(Y, M, [falsy(or(A, B))|P], S).

tree([truthy(imply(A, B))|X], node(truthy(imply(A, B)), [L, R]), P, S) :-
    call(S, [falsy(A)|X], Y1),
    call(S, [truthy(B)|X], Y2),
    tree(Y1, L, [truthy(imply(A, B))|P], S),
    tree(Y2, R, [truthy(imply(A, B))|P], S).

tree([falsy(imply(A, B))|X], node(falsy(imply(A, B)), [M]), P, S) :-
    include(truthy, X, Y),
    include(truthy, P, Q),
    call(S, [truthy(A), falsy(B)|Y], Z),
    tree(Z, M, Q, S).

tree([truthy(A)|X], node(truthy(A), [M]), P, S) :-
    atom(A),
    tree(X, M, [truthy(A)|P], S).

tree([falsy(A)|X], node(falsy(A), [M]), P, S) :-
    atom(A),
    tree(X, M, [falsy(A)|P], S).

tree([], open, _, _).

closed(node(_, X)) :-
    forall(member(Y, X), closed(Y)).

closed(close).

probable(A, C, S) :-
    maplist(truthy, A, X),
    maplist(falsy, C, Y), 
    forall(member(Z, Y), (tree([Z|X], T, [], S), closed(T))).

