:- module(ipc, [ipc_probable/2]).
:- use_module(library(lists)).

path([[and(A, B), W, t]|X], [[and(A, B), W, t]|P]) :-
	path([[A, W, t], [B, W, t]|X], P).
path([[and(A, B), W, f]|X], [[and(A, B), W, f]|P]) :-
	path([[A, W, f]|X], P);
	path([[B, W, f]|X], P).
path([[or(A, B), W, t]|X], [[or(A, B), W, t]|P]) :-
	path([[A, W, t]|X], P);
	path([[B, W, t]|X], P).
path([[or(A, B), W, f]|X], [[or(A, B), W, f]|P]) :-
	path([[A, W, f], [B, W, f]|X], P).
path([[imply(A, B), W, t]|X], [[imply(A, B), W, t]|P]) :-
	V is W+1,
	(path([[A, V, f]|X], P);
	 path([[B, V, t]|X], P)).
path([[imply(A, B), W, f]|X], [[imply(A, B), W, f]|P]) :-
	V is W+1,
	path([[A, V, t], [B, V, f]|X], P).
path([[not(A), W, t]|X], [[not(A), W, t]|P]) :-
	V is W+1,
	path([[A, V, f]|X], P).
path([[not(A), W, f]|X], [[not(A), W, f]|P]) :-
	V is W+1,
	path([[A, V, t]|X], P).
path([[A, W, t]|X], [[A, W, t]|P]) :-
	atom(A), 
	path(X, P).
path([[A, W, f]|X], [[A, W, f]|P]) :-
	atom(A), 
	path(X, P).
path([], []).

inconsistent(P) :-
	member([A, W, t], P), 
	member([A, Z, f], P),
	W =< Z.

truthy(P, Q) :-
	Q=[P, 0, t].

falsy(P, Q) :-
	Q=[P, 0, f].

ipc_probable(Assumptions, Conclusions) :-
	maplist(truthy, Assumptions, A),
	maplist(falsy, Conclusions, C),
	forall(member(D, C), forall(path([D|A], P), inconsistent(P))).
