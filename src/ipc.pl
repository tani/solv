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

:- module(ipc, [ipc_probable/2]).
:- use_module(library(lists)).

shuffle(X, Y) :-
    append([L, [M], R], X),
    append([[M], L, R], Y).
shuffle([], []).

reach(S, S, R) :-
    flatten(R, Ws),
    list_to_set(Ws, Vs),
    member(S, Vs).
reach(S, T, R) :-
    member([S, T], R).
reach(S, T, R) :-
    member([S, U], R),
    reach(U, T, R).

tree([label(and(A, B), W, t)|X], node(label(and(A, B), W, t), [C]), R) :-
    shuffle([label(A, W, t), label(B, W, t)|X], Y),
    tree(Y, C, R).

tree([label(and(A, B), W, f)|X], node(label(and(A, B), W, f), [C1, C2]), R) :-
    shuffle([label(A, W, f)|X], Y1),
    shuffle([label(B, W, f)|X], Y2),
    tree(Y1, C1, R),
    tree(Y2, C2, R).

tree([label(or(A, B), W, t)|X], node(label(or(A, B), W, t), [C1, C2]), R) :-
    shuffle([label(A, W, t)|X], Y1),
    shuffle([label(B, W, t)|X], Y2),
    tree(Y1, C1, R),
    tree(Y2, C2, R).

tree([label(or(A, B), W, f)|X], node(label(or(A, B), W, f), [C]), R) :-
    shuffle([label(A, W, f), label(B, W, f)|X], Y),
    tree(Y, C, R).

tree([label(imply(A, B), W, t)|X], node(label(imply(A, B), W, t), [C1, C2]), R) :-
    findall(L, (reach(W, V, R), L=label(A, V, f)), Y1),
    findall(L, (reach(W, V, R), L=label(B, V, t)), Y2),
    append(X, Y1, Z1),
    append(X, Y2, Z2),
    shuffle(Z1, U1),
    shuffle(Z2, U2),
    tree(U1, C1, R),
    tree(U2, C2, R).

tree([label(imply(A, B), W, f)|X], node(label(imply(A, B), W, f), [C]), R) :-
    gensym(w_, V),
    shuffle([label(A, V, t), label(B, V, f)|X], Y),
    tree(Y, C, [[W,V]|R]).

tree([label(not(A), W, t)|X], node(label(not(A), W, t), [C]), R) :-
    findall(L, (reach(W, V, R), L=label(A, V, f)), Y),
    append(X, Y, Z),
    shuffle(Z, U),
    tree(U, C, R).

tree([label(not(A), W, f)|X], node(label(not(A), W, f), [C]), R) :-
    gensym(w_, V),
    shuffle([label(A, V, t)|X], Y),
    tree(Y, C, [[W, V]|R]).

tree([label(A, W, t)|X], node(label(A, W, t), [C]), R) :-
    atom(A), 
    findall(L, (member([W, V], R), L=label(A, V, t)), Y),
    append(X, Y, Z),
    shuffle(Z, U),
    tree(U, C, R).

tree([label(A, W, f)|X], node(label(A, W, f), [C]), R) :-
    atom(A), 
    tree(X, C, R).

tree([], end, _).

path(node(L, Cs), [L|P]) :-
    member(C, Cs),
    path(C, P).
path(end, []).

inconsistent(P) :-
    member(label(A, W, t), P), 
    member(label(A, W, f), P).

truthy(P, Q) :-
    Q=label(P, w_0, t).

falsy(P, Q) :-
    Q=label(P, w_0, f).

ipc_probable(Assumptions, Conclusions) :-
    maplist(truthy, Assumptions, A),
    maplist(falsy, Conclusions, C),
    forall(member(D, C), (tree([D|A], T, []), forall(path(T, P), inconsistent(P)))).
