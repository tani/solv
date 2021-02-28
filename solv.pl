#!/usr/bin/env swipl

:- use_module('ipc.pl').
:- use_module('cpc.pl').
:- initialization(main, main).

probable(System, Assumptions, Conclusions) :-
	System = ipc -> ipc_probable(Assumptions, Conclusions);
	System = cpc -> cpc_probable(Assumptions, Conclusions).

main(Argv) :-
	maplist(term_to_atom, [System,Conclusion|Assumptions], Argv),
	(probable(System, Assumptions, [Conclusion])
	 -> (write(probable), halt(0))
          ; (write(unprobable), halt(1))).

