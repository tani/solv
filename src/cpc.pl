% -*- mode: prolog -*-
%
shuffle(X, Y) :-
    append([L, [M], R], X),
    append([[M], L, R], Y).
shuffle([], []).

tree([truthy(A)|_], node(truthy(A), [close]), P) :-
    member(falsy(A), P), !.

tree([falsy(A)|_], node(falsy(A), [close]), P) :-
    member(truthy(A), P), !.

tree([truthy(not(A))|X], node(truthy(not(A)), [M]), P) :-
    shuffle([falsy(A)|X], Y),
    tree(Y, M, [truthy(not(A))|P]).

tree([falsy(not(A))|X], node(falsy(not(A)), [M]), P) :-
    shuffle([truthy(A)|X], Y),
    tree(Y, M, [falsy(not(A))|P]).

tree([truthy(and(A, B))|X], node(truthy(and(A, B)), [M]), P) :-
    shuffle([truthy(A), truthy(B)|X], Y),
    tree(Y, M, [truthy(and(A, B))|P]).

tree([falsy(and(A, B))|X], node(falsy(and(A, B)), [L, R]), P) :-
    shuffle([falsy(A)|X], Y1),
    shuffle([falsy(B)|X], Y2),
    tree(Y1, L, [falsy(and(A, B))|P]),
    tree(Y2, R, [falsy(and(A, B))|P]).

tree([truthy(or(A, B))|X], node(truthy(or(A, B)), [L, R]), P) :-
    shuffle([truthy(A)|X], Y1),
    shuffle([truthy(B)|X], Y2),
    tree(Y1, L, [truthy(or(A, B))|P]),
    tree(Y2, R, [truthy(or(A, B))|P]).

tree([falsy(or(A, B))|X], node(falsy(or(A, B)), [M]), P) :-
    shuffle([truthy(A), truthy(B)|X], Y),
    tree(Y, M, [falsy(or(A, B))|P]).

tree([truthy(imply(A, B))|X], node(truthy(imply(A, B)), [L, R]), P) :-
    shuffle([falsy(A)|X], Y1),
    shuffle([truthy(B)|X], Y2),
    tree(Y1, L, [truthy(imply(A, B))|P]),
    tree(Y2, R, [truthy(imply(A, B))|P]).

tree([falsy(imply(A, B))|X], node(falsy(imply(A, B)), [M]), P) :-
    shuffle([truthy(A), falsy(B)|X], Y),
    tree(Y, M, [falsy(imply(A, B))|P]).

tree([truthy(A)|X], node(truthy(A), [M]), P) :-
    atom(A),
    tree(X, M, [truthy(A)|P]).

tree([falsy(A)|X], node(falsy(A), [M]), P) :-
    atom(A),
    tree(X, M, [falsy(A)|P]).

tree([], open, _).

closed(node(_, X)) :-
    forall(member(Y, X), closed(Y)).

closed(close).

truthy(X, Y) :- Y=truthy(X).

falsy(X, Y) :- Y=falsy(X).

prove(A, C) :-
    maplist(truthy, A, X),
    maplist(falsy, C, Y), 
    forall(member(Z, Y), (tree([Z|X], T, []), closed(T))).
