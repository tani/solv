path([[and(A, B), W, t]|X], R, [[and(A, B), W, t]|P]) :-
	path([[A, W, t], [B, W, t]|X], R, P).
path([[and(A, B), W, f]|X], R, [[and(A, B), W, f]|P]) :-
	path([[A, W, f]|X], R, P);
	path([[B, W, f]|X], R, P).
path([[or(A, B), W, t]|X], R, [[or(A, B), W, t]|P]) :-
	path([[A, W, t]|X], R, P);
	path([[B, W, t]|X], R, P).
path([[or(A, B), W, f]|X], R, [[or(A, B), W, f]|P]) :-
	path([[A, W, f]|X], R, P), 
	path([[B, W, f]|X], R, P).
path([[imply(A, B), W, t]|X], R, [[imply(A, B), W, t]|P]) :-
	path([[A, W, f]|X], R, P);
	path([[B, W, t]|X], R, P).
path([[imply(A, B), W, f]|X], [[W,V]|R], [[imply(A, B), W, f]|P]) :-
	gensym(w, V), 
	path([[A, V, t], [B, V, f]|X], R, P).
path([[not(A), W, t]|X], R, [[not(A), W, t]|P]) :-
	path([[A, W, f]|X], R, P).
path([[not(A), W, f]|X], [[W,V]|R], [[not(A), W, f]|P]) :-
	gensym(w, V), 
	path([[A, V, t]|X], R, P).
path([[A, W, t]|X], R, [[A, W, t]|P]) :-
	atom(A), 
	path(X, R, P).
path([[A, W, f]|X], R, [[A, W, f]|P]) :-
	atom(A), 
	path(X, R, P).
path([], [], []).

reach(S, T, R) :-
	member([S, T], R);
	member([S, U], R), 
	reach(S, U, R), 
	reach(U, T, R).
reacheq(S, S, _).
reacheq(S, T, R) :- reach(S, T, R).

inconsistent(P, R) :-
	member([A, W, t], P), 
	member([A, Z, f], P),
	reacheq(W, Z, R).
