%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2014 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: util.m.
% Main author: c4cypher.
% Stability: low.
% 
% Utility predicates for working with Lua.
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module template.

:- interface.

:- import_module float.
:- import_module int.
:- import_module string.
:- import_module char.


% Dynamically cast string and numeric values to their equivalents.

:- func cast_to_string(T) = string is semidet.
:- func cast_to_float(T) = float is semidet.
:- func cast_to_int(T) = int is semidet.
:- func cast_to_char(T) = char is semidet.

:- implementation.

cast_to_string(T) =
	string.string(T).

cast_to_float(T) = 
	( dynamic_cast(T, F:float) -> F
	; dynamic_cast(T, I:int), -> float(I)
	; to_float(cast_to_string(T), F) -> F
	; fail
	).
		
cast_to_int(T) =
	( dynamic_cast(T, I:int) -> I
	; dynamic_cast(T, F@float(truncate_to_int(F)@I)) -> I 
	; to_int(cast_to_string(T), I) -> I
	; fail
	).
	
cast_to_char(T) = C :-
	cast_to_string(T),
	length(S) = 1,
	det_index(S, 1, C).

