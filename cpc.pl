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
