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

%-----------------------------------------------------------------------------%
%
% The Stack type
%

% Normally, when Mercury comes into contact with mutable data structures;
% usually when interacting with foreign code; Mercury either has to pretend
% said structure is immutable, interact with it using an io__state, or handle
% the onerous task of interacting with the data structure using impure calls.
%
% Stack data structures, (and other similar structures like cons lists) are an
% interesting case for this scenario, as 


	% Polymorphic type for handling impure stacks in a lazy, but pure
	% manner.  The committed form caches the number of values to pop in
	% order to return the stack to it's original form.
	%
:- type stack(T)
	---> 	some [S] (impure_stack(S) => impure_stack(S, T))
	;	stack(stack(T), T)
	;	partial_stack(stack(T), int)
	;	comitted_stack(int) 	
	;	reduced_stack(int)
	where equality is equal_stacks.	

:- inst pure_stack 
	---> 	stack(ground)
	;	stack(unique)
	;	stack(mostly_unique)
	; 	partial_stack(ground, ground)
	;	partial_stack(unique, ground)
	;	partial_stack(mostly_unique, ground).

:- inst impure_stack 
	---> 	impure_stack(ground)
	;	impure_stack(unique)
	;	impure_stack(mostly_unique)
	; 	comitted_stack(ground)
	; 	reduced_stack(ground).

:- mode pi == pure_stack >> pure_stack.
:- mode po == free >> pure_stack.

:- mode commit == pure_stack >> impure_stack.
:- mode revert == impure_stack >> pure_stack.

:- mode co == commit.
:- mode re == revert.

%-----------------------------------------------------------------------------%
%
%  Stack operations.
%

	% Retreive the int at the top of the stack, 
	% starting at 1 for the bottom.
	% Fails if the stack is empty.
	%
:- func top(stack(T)) = int is semidet.

	% Retreive the number of values on the stack.
	% Effectively a det version of top.
	%
:- func count(stack(T)) = int is det.
	
	% Succeed if the given int is valid.
	%
:- pred valid_index(int:in, stack(T)::in) is semidet.
	
	% Pop a value off the stack.
	%
:- func pop(stack(T)) = stack(T) is det.
	
	% Pop a number of values off of the stack.
	%
:- func pop(int, stack(T)) = stack(T).
:- mode pop(in, in) = out is det.
:- mode pop(out, in) = in is semidet.
:- mode pop(out, in) = out is nondet.

	% retreive the value at the given int (Starting at 1 for the bottom).
	%
:- func index(int, stack(T)) = V <= impure_stack(T, V).
:- mode index(in, in) = out is semidet.
:- mode index(out, in) = in is semidet.
:- mode index(out, in) = out is nondet.
	

	% Push a value onto the stack.
	%
:- func push(V, stack(T)) = stack(T) <= impure_stack(T, V).
:- mode push(in, in) = out is det.
:- mode push(out, out) = in is det.


:- pred equal_stacks(stack(T)::in, stack(T)::in) <= impure_stack(T, V)
	is semidet.


:- impure pred commit(stack(T), T).
:- 	  mode commit(commit, out) is det.
:- 	  mode commit(commit, uo) is det.
:- 	  mode commit(commit, muo) is det.

	% Return the stack to it's pure form.
	%
:- impure pred revert(T, stack(T)).
:-	  mode revert(in, revert) is det.
:-	  mode revert(di, revert) is det.
:-	  mode revert(mdi, revert) is det.


%-----------------------------------------------------------------------------%
%
%  Impure stack typeclass
%

% TODO: Talk about this


:- typeclass impure_stack(T) where [

	% retreive the int for the top value of the stack, fails if empty.
	%
	semipure func semipure_top(T) = int is semidet,
	
		% Check to see if a given int is valid.
	%
	semipure pred semipure_valid(int, T) is semidet,
	
	% Ensure that it is safe to push a number of values onto the stack.
	% Should fail the space for the new values cannot be allocated.
	%
	impure pred impure_free(T::in, int::in) is semidet,
	
	% The error to call when impure_free fails.
	%
	semipure pred stack_allocation_error(T::in) is erroneus,
	
		% Pop a number of values off the stack.
	%
	impure pop impure_pop(T, int),
	mode impure_pop(in, in) is det,
	
	% The error to call when one tries to pop off more values than exist.
	%
	semipure pred stack_pop_error(T::in) is erroneus, 
	

].


:- typeclass impure_stack(T) <= impure_stack(T) where [


	% retreive the value at the given int (Ttarting at 1 for the bottom).
	%
	semipure func semipure_index(int, T) = V is semidet,
	
	% Push a value onto the stack.
	%
	impure pred impure_push(T, V),
	mode impure_push(in, in) is det,
	
	
].



%-----------------------------------------------------------------------------%
%
% Polymorphic impure stacks
%

	% A polymorphic form of impure_stack, using a higher level type
	% to store the more specific type polymorphically.
	%
:- typeclass impure_stack(T, U, V) <= (
		(T -> U), (V -> U), 
		impure_stack(T), 
		impure_stack(T, U)
) where [
	
	% Cast the more specific type to the more general type.
	%
	func upcast(V) = U,
	mode upcast(in) = out is det,
	
	% Cast the more general type to the more specific type, fail if
	% V does not match the type of the value stored by U
	func downcast(U) = V,
	mode downcast(in) = out is semidet

].



%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.


% :- func top(stack(T)) = int <= impure_stack(T) is semidet.


% :- func count(stack(T)) = int <= impure_stack(T) is det.
	

% :- pred valid_index(int:in, stack(T)::in) <= impure_stack(T) is semidet.
	

% :- func pop(stack(T)) = stack(T) <= impure_stack(T) is det.
	

% :- func pop(int, stack(T)) = stack(T) <= impure_stack(T).
% :- mode pop(in, in) = out is det.
% :- mode pop(out, in) = in is semidet.
% :- mode pop(out, in) = out is nondet.


% :- func index(int, stack(T)) = V <= impure_stack(T).
% :- mode index(in, in) = out is semidet.
% :- mode index(out, in) = in is semidet.
% :- mode index(out, in) = out is nondet.
	


% :- func push(V, stack(T)) = stack(T) <= impure_stack(T).
% :- mode push(in, in) = out is det.
% :- mode push(out, out) = in is det.


% :- pred equal_stacks(stack(T)::in, stack(T)::in) <= impure_stack(T)
% 	is semidet.


% :- impure pred commit(stack(T), T) <= impure_stack(T).
% :- 	  mode commit(commit, out) is det.
% :- 	  mode commit(commit, uo) is det.
% :- 	  mode commit(commit, muo) is det.


% :- impure pred revert(T, stack(T)) <= impure_stack(T).
% :-	  mode revert(in, revert) is det.
% :-	  mode revert(di, revert) is det.
% :-	  mode revert(mdi, revert) is det.



