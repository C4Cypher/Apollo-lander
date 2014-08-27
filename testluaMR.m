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

:- pragma foreign_import_module("C", luaMR).

main(!IO) :-
	L = lua_new, 
	impure lua_pushvalue(L, globalindex),
	impure lua_getfield(L, index(-1), "tostring"),
	impure lua_pushuserdata(L, ["A","B", "C"]),
	impure _ = lua_call(L, 1, 1),
	semipure S = lua_tostring(L, index(-1)),
	print(S, !IO).

:- pragma promise_pure(main/2).



