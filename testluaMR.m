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

:- pred main(io::di, io::uo) is det.


:- implementation.

:- import_module univ.
:- import_module list.
:- import_module exception.

:- import_module luaMR.
:- import_module luaMR.api.
:- import_module float.
:- import_module int.

:- pragma foreign_import_module("C", luaMR).

main(!IO) :-
	L = lua_new, 
	impure lua_pushfunc(L, add),
	impure lua_pushinteger(L, 1),
	impure lua_pushinteger(L, 1),
	impure lua_call(L, 2, 1),
	semipure Result = lua_tointeger(L, index(-1)) ->
		print(Result, !IO) 
	; 
		print("Not int", !IO).

:- pragma promise_pure(main/2).

:- impure func add(lua) = int.

add(L) = 1 :-
	semipure A = lua_tonumber(L, index(1)),
	semipure B = lua_tonumber(L, index(2)),
	impure lua_pushnumber(L, A + B).
	
	

