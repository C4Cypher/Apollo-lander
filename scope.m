%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2014 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: index.m.
% Main author: C4Cypher.
% Stability: low.
% 
% A set of indexable types that emulate lexical scope for dynamically typed
% variable assignments.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module scope.

:- interface.

:- import_module index.

:- import_module type_desc.
:- import_module map.

%-----------------------------------------------------------------------------%
%
% Scope
%

	% The scope type represents a set of dynamic variable assignments
	% bound by type T in a given context.
	% 
:- type scope.



	% Produces a global scope without any variable bindings.
	%
:- func empty_scope = scope.


	% Take an indexed value and use it as the initial global scope.
	% Changes made to the global scope will be interned.  This makes a
	% 'weak' global scope, as the internal representation of the global
	% scope 
	%
:- func new_scope(T) = scope <= (index(T, _K)).

	% Retreive the global

%-----------------------------------------------------------------------------%
%
% Accessing and modifying scope values.
%

	% get(Key, Value, Scope):
	% 
	% Retreive Value indexed by Key within Scope.
	% Fail on type mismatch.
	%
:- pred get(K, V, scope).
:- mode get(in, out, in) is semidet.
:- mode get(out, out, in) is nondet.

:- pred set(K::in, V::in, scope::in, scope::out) is det.

%-----------------------------------------------------------------------------%


:- instance index(scope(T), univ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- interface.


:- type global_index(T)
	---> 	some [S] (global_scope(S) => (free(S, T), new_index(S, T)))
	;	global_scope(global_scope(T), map(univ, T)).





