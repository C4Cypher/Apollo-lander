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
:- import_module pair.
:- import_module assoc_list.
:- import_module exception.
:- import_module solutions.

:- import_module trail.
:- import_module luaMR.
:- import_module luaMR.api.
:- import_module luaMR.state.

:- pragma foreign_import_module("C", luaMR).

main(!IO) :-
	some [LS] (
		new_state(LS0),
		init_lua(LS0, LS1),
		%table(local(globalindex), pairs, [], List, LS1, _), Suddenly the table mess makes more sense, starting over
		%String = string.string(List),
		print("Hello World", !IO)
	).
	
	
	
	
%:- pragma promise_pure(main/2).

:- pred pairs(value::in, value::in, assoc_list(value)::in, assoc_list(value)::out) is det.

pairs(K, V, List, [(K - V) | List]).
	



