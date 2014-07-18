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

% S refers to mutable Stack type
% V refers to values on the stack

	% Intended syntax: stack(S, int), X = (stack(Y), 1, 2, 3, 4, 5, 6)  
	% ... this may not work.
	% Intended to be shorthand
:- type stack(S, V)
	---	stack(Stack)
	;	stack(Stack, Value) , V.

	% Polymorphic stack. 
	%
:- type stack(S) == stack(S, _).
	
:- type index == int.	
	


	% Retreive the index at the top of the stack, (starting at 1 for the bottom).
	% Fails if the stack is empty.
	%
:- func top(stack(S, V)) = index <= impure_stack(S, V) is semidet.

	% Retreive the number of values on the stack.
	% Effectively a det version of top.
	%
:- func count(stack(S, V)) = int <= impure_stack(S, V) is det.
	
	% retreive the value at the given index (starting at 1 for the bottom).
	%
:- func index(int, stack(S, V)) = V <= impure_stack(S, V) is semidet.
	
	% Succeed if the given index is valid.
	%
:- pred valid_index(index::in, stack(S, V)::in) <= impure_stack(S, V) is semidet.
	
	% Push a value onto the stack.
	%
:- func push(V, stack(S, V)) = stack(S, V) is det.

	% Pop a number of values off of the stack.
	%
:- func pop(int, stack(S, V)) = stack(S, V) is det.

:- func safe_pop(int, stack(S, V)) = stack(S, V) is semidet.




:- typeclass impure_stack(S, V) where [

	% retreive the index for the top value of the stack, fails if empty.
	%
	semipure func semipure_top(S) = int is semidet,
	
	% retreive the value at the given index (starting at 1 for the bottom).
	%
	semipure func semipure_index(int, S) = V is semidet,
	
	% Check to see if a given index is valid.
	%
	semipure pred semipure_valid(int, S) is semidet,
	
	% Ensure that it is safe to push a number of values onto the stack.
	% Should fail the space for the new values cannot be allocated.
	%
	impure pred impure_free(S::in, int::in) is semidet,
	
	% The error to call when impure_free fails.
	%
	semipure pred stack_allocation_error(S::in) is erroneus,
	
	% Push a value onto the stack.
	%
	impure pred impure_push(S, V),
	mode impure_push(in, in) is det,
	
	% Pop a number of values off the stack.
	%
	impure pop impure_pop(S, int),
	mode impure_pop(in, in) is det,
	
	% The error to call when one tries to pop off more values than exist.
	%
	semipure pred stack_pop_error(S::in) is erroneus, 
	
	% Make a copy of a stack
	%
	semipure copy_stack(S) = S
].

:- instance impure_stack(stack(S, V)) <= impure_stack(S, V).

:- instance impure_stack(stack(S)) <= impure_stack(S, _).


:- implementation.

