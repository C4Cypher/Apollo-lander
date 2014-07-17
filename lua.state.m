%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2014 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: lua.state.m.
% Main author: C4Cypher.
% Stability: low.
% 
% This file defines the lua_state typeclass.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%


:- module lua.state.

:- interface.


:- type index == int.

:- typeclass lua_value(L, V) where [
	pred push(V, L, L),
	mode push(in, mdi, muo) is 

:- typeclass lua_state(L) where [
	pred new(L),
	mode new(muo) is det.
	
	pred stack
	
	func call_function(F, list(V), list(V), ) <= 
	
