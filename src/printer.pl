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

:- module(printer, [print_tree/1]).

print_tree(I, [node(L, C)]) :- !,
	maplist(write, I),
	write('┗━ '),
	write(L),nl,
	append(I, ['   '], J),
	print_tree(J, C).
print_tree(I, [L]) :- !,
	maplist(write, I),
	write('┗━ '),
	write(L),nl.
print_tree(I, [node(L, C)|N]) :- !,
	maplist(write, I),
	write('┣━ '),
	write(L),nl,
	append(I, ['┃  '], J),
	print_tree(J, C),
	print_tree(I, N).
print_tree(I, [L|N]) :- !,
	maplist(write, I),
	write('┣━ '),
	write(L),nl,
	print_tree(I, N).
print_tree(_, []).
print_tree(node(L, C)) :- !,
	write(L),nl,
	print_tree([], C).
