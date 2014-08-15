%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2014 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: scope.m.
% Main author: c4cypher.
% Stability: low.
% 
% Defines types and typeclasses for handling variable scope binding in a 
% purely declarative manner, specifically with the semantics of lexicaly bound
% block scope
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module scope.

%-----------------------------------------------------------------------------%
%
% The Scope typeclass
%

:- typeclass scope(Scope, Name, Value) where [

	% Bind a Value to a variable Name
	pred bind(Name, Value, Scope, Scope),
	mode bind(in, in, in, out) is det,		% assignment
	mode bind(in, out, out, in) is semidet, 	% unassignment
	mode bind(in, out, unused, in) is semidet	% lookup
	mode bind(out, out, out, in) is nondet,	
	
].


	% bound(N, V, S) :- bind(N, V, _, S).
	%
:- pred bound(Name, Value, Scope) <= scope(Scope, Name, Value).
:- mode bound(in, out, in) is semidet.
:- mode bound(out, out, in) is nondet.

	% lookup
	%
:- func Scope ^ Name = Value <= scope(Scope, Name, Value).
:- mode in    ^ in   = out is semidet.
:- mode in    ^ out  = out is nondet.

	% assignment
	%
:- func (Scope ^ Name := Value) = Scope <= (Scope, Name, Value).
:- mode (in    ^ in   := in   ) = out is det.
:- mode (out   ^ out  := out   ) = in is nondet.


%-----------------------------------------------------------------------------%
%
% Nested scope 
%

	% Nested scopes
:- typeclass nested_scope(Child) <= (scope(Child, N, V), scope(Parent, N, V))
	where [
	
	pred parent(Child, Parent),
	mode parent(in, out) is det,
	mode parent(out, in) is det
].

:- type scope(T).

:- instance scope(scope(T), N, V) <= scope(T, N, V).
:- 
	% Open a nested variable scope






	
	
