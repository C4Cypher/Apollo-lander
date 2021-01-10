%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2014 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: test.m.
% Main author: c4cypher.
% Stability: low.
% 
% Test the apollo library.
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module test.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.


:- implementation.

:- import_module float.
:- import_module int.
:- import_module string.
:- import_module univ.
:- import_module list.
:- import_module pair.
:- import_module assoc_list.
:- import_module exception.
:- import_module require.
:- import_module solutions.

:- import_module trail.
:- import_module apollo.
:- import_module apollo.api.
:- import_module apollo.state.

:- pragma foreign_import_module("C", apollo).

main(!IO) :-
	new_state(L0),
  HelloLua = string_to_func("print ""Hello World!""", L0, L1),
  call_lua_func(HelloLua, [], _, L1, _).
	
	
	
	



