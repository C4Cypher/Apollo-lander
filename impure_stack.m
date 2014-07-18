%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2014 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: impure_stack.m.
% Main author: C4Cypher.
% Stability: low.
% 
% This library defines types and typeclasses for handling operations on 
% mutable stack data structures with pure declarative semantics. 
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module impure_stack.

:- interface.

:- import_module int.

	
:- type index == int.	
	
:- type stack(T, V)
	---> 	empty_stack(T)
	;	stack(stack_cons(T, V)).
	
:- type stack_cons(T, V)
	---> 	cons(stack_cons(T, V))
	;	T , V.

:- type stack(T) == stack(stack(T), univ).
	

:- type stack_value(T)
	---> some [V] ( value(V) => stack(T, V) ).


:- type stack == stack(stack, stack_value).

:- type stack_value == stack_value(stack).

:- instance stack(stack(T, V)).

:- instance stack(stack).
:- instance stack(stack, stack_value).



:- typeclass stack(S) where [

	% Retreive the index at the top of the stack, (starting at 1 for the bottom).
	% Fails if the stack is empty.
	%
	func top(S)
	mode top(in) is semidet,

	% Retreive the number of values on the stack.
	% Effectively a det version of top.
	%
	func count(S) = int
	mode count(in) = out is det,
	
	% Succeed if the given index is valid.
	%
	pred valid_index(index, S),
	mode valid_index(in, in) is semidet,
	
	% Pop a value off the stack.
	%
	func pop(S) = S,
	mode pop(in) = out is det,
	
	% Pop a number of values off of the stack.
	%
	func pop(int, S) = S
	mode pop(in, in) = out is det,
	

	% Pop values off the stack, failing if there aren't enough values to pop.
	%
	func safe_pop(int, S) = S is semidet,


].

:- typeclass stack(S, V) <= stack(S) where [

	% retreive the value at the given index (starting at 1 for the bottom).
	%
	func index(index, S) = V
	mode index(in, in) = out is semidet,
	

	% Push a value onto the stack.
	%
	func push(V, S) = S,
	mode push(in, in) = out is det,
	mode push(out, out) = in is det,


].




%-----------------------------------------------------------------------------%

:- typeclass impure_stack(S) where [

	% retreive the index for the top value of the stack, fails if empty.
	%
	semipure func semipure_top(S) = int is semidet,
	
		% Check to see if a given index is valid.
	%
	semipure pred semipure_valid(index, S) is semidet,
	
	% Ensure that it is safe to push a number of values onto the stack.
	% Should fail the space for the new values cannot be allocated.
	%
	impure pred impure_free(S::in, int::in) is semidet,
	
	% The error to call when impure_free fails.
	%
	semipure pred stack_allocation_error(S::in) is erroneus,
	
		% Pop a number of values off the stack.
	%
	impure pop impure_pop(S, int),
	mode impure_pop(in, in) is det,
	
	% The error to call when one tries to pop off more values than exist.
	%
	semipure pred stack_pop_error(S::in) is erroneus, 
	

].


:- typeclass impure_stack(S, V) <= impure_stack(S) where [


	% retreive the value at the given index (starting at 1 for the bottom).
	%
	semipure func semipure_index(index, S) = V is semidet,
	
	% Push a value onto the stack.
	%
	impure pred impure_push(S, V),
	mode impure_push(in, in) is det,
	
	
].




