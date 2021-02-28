reach(S, T, R) :-
	member([S, T], R);
	member([S, U], R), 
	reach(S, U, R), 
	reach(U, T, R).
reacheq(S, S, _).
reacheq(S, T, R) :- reach(S, T, R).

path([[and(A, B), W, t]|X], R, [[and(A, B), W, t]|P], Z) :-
	path([[A, W, t], [B, W, t]|X], R, P, Z).
path([[and(A, B), W, f]|X], R, [[and(A, B), W, f]|P], Z) :-
	path([[A, W, f]|X], R, P, Z);
	path([[B, W, f]|X], R, P, Z).
path([[or(A, B), W, t]|X], R, [[or(A, B), W, t]|P], Z) :-
	path([[A, W, t]|X], R, P, Z);
	path([[B, W, t]|X], R, P, Z).
path([[or(A, B), W, f]|X], R, [[or(A, B), W, f]|P], Z) :-
	path([[A, W, f]|X], R, P, Z), 
	path([[B, W, f]|X], R, P, Z).
path([[imply(A, B), W, t]|X], R, [[imply(A, B), W, t]|P], Z) :-
	reacheq(W, V, R), 
	(path([[A, V, f]|X], R, P, Z);
	 path([[B, V, t]|X], R, P, Z)).
path([[imply(A, B), W, f]|X], R, [[imply(A, B), W, f]|P], Z) :-
	gensym(w, V), 
	path([[A, V, t], [B, V, f]|X], [[W, V]|R], P, Z).
path([[not(A), W, t]|X], R, [[not(A), W, t]|P], Z) :-
	reacheq(W, V, R), 
	path([[A, V, f]|X], R, P, Z).
path([[not(A), W, f]|X], R, [[not(A), W, f]|P], Z) :-
	gensym(w, V), 
	path([[A, V, t]|X], [[W, V]|R], P, Z).
path([[A, W, t]|X], R, [[A, W, t]|P], Z) :-
	atom(A), 
	path(X, R, P, Z).
path([[A, W, f]|X], R, [[A, W, f]|P], Z) :-
	atom(A), 
	path(X, R, P, Z).
path([], W, [], W).

inconsistent(P, R) :-
	member([A, W, t], P), 
	member([A, Z, f], P),
	reacheq(W, Z, R).
