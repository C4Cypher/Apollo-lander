%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2014 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: testluaMR.m.
% Main author: c4cypher.
% Stability: low.
% 
% Test the luaMR library.
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module testluaMR.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.


:- implementation.

:- import_module float.
:- import_module int.
:- import_module string.
:- import_module univ.
:- import_module list.
:- import_module exception.

:- import_module trail.
:- import_module luaMR.
:- import_module luaMR.api.
:- import_module luaMR.state.

:- pragma foreign_import_module("C", luaMR).

main(!IO) :-
	LS0 = lua_state(L@lua_new, current_id, empty_trail),
	impure lua_pushinteger(L, 1), 
	not (
		semipure set(2, LS0, ls(L1, _, _)),
		impure lua_pushnil(L),
		impure lua_pop(L, 1), 
		semipure three(L1),
		fail
	),
	LS0 = ls(L2, _, _),
	semipure I = lua_tointeger(L2, index(1)),
	print(I, !IO).
	
	
:- pragma promise_pure(main/2).

:- semipure pred set(int::in, ls::mdi, ls::muo) is det.

set(I, ls(L, Idx, Trail), LS) :-
	semipure I0 = lua_tointeger(L, index(1)),
	F = unset(I0),
	impure trail_if_newer(F, ls(L, Idx, Trail), LS),
	impure lua_pushinteger(L, I),
	impure lua_replace(L, index(1)).
	

:- pragma promise_semipure(set/3).	

:- impure func unset(int, lua) = int.

unset(I, L) = 0 :-
	impure lua_pushinteger(L, I),
	impure lua_replace(L, index(1)).

:- semipure pred three(lua::in) is semidet.

three(L) :- semipure N = lua_tointeger(L, index(1)), N = 3.

