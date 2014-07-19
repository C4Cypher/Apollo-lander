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

:- module pure_stack.

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
:- type stack(T, V)
	---> 	stack(T)
	;	stack(stack(T, V), V)
	;	partial_stack(T, int)
	;	impure_stack(stack(T, V), int)
	;	impure_stack(int). 	% For handling unique insts.

%-----------------------------------------------------------------------------%
%
% The Stack type
%

% These calls are identical to those in the stack(T, V) typeclass, and should
% be used for implementing instances of stack(T, V).

:- pred equal_stacks(stack(T, V)::si, stack(T, V)::si) <= pure_stack(T, V)
	is semidet.

:- func stack_push(V, stack(T, V)) = stack(T, V) <= pure_stack(T, V) is det.

:- func stack_top(stack(T, V))
	% Commit the stack's changes, and expose the raw mutable type for
	% impure queries.
	%
:- impure pred commit(stack(T, V), T) <= pure_stack(T, V).
:- 	  mode commit(commit, out) is det.
:- 	  mode commit(commit, uo) is det.
:- 	  mode commit(commit, muo) is det.

	% Return the stack to it's pure form.
	%
:- impure pred revert(T, stack(T, V)) <= pure_stack(T, V).
:-	  mode revert(in, revert) is det.
:-	  mode revert(di, revert) is det.
:-	  mode revert(mdi, revert) is det.


 
%-----------------------------------------------------------------------------%
%
% Stack modes
%

	% In and out modes for pure_stacks.
	%
:- mode si == in(pure_stack).
:- mode so == out(pure_stack).


	% Mode for the moment a stack is readied for impure operations.
	%
:- mode commit == pure_stack >> impure_stack.

	% Mode for the moment when a stack's values are committed to the stack.
	%
:- mode commit_push == lazy_stack >> comitted_stack.


	% Mode for the moment a partial stack is prepared for impure operations.
	%
:- mode commit_reduce == partial_stack >> reduced_stack.
	
	% Mode for the moment when a stack is returned to the state it was at
	% when it was committed.
	%
:- mode revert == comitted_stack >> pure_stack.

	% Mode for the moment when the values of a committed stack are popped
	% off of the stack, returning it to it's pure form.
	% 
:- mode revert_pop == impure_stack >> lazy_stack.

	% Reverting a reduced stack doesn't actually do much to the internal
	% state. So this simply returns a reduced stack to it's partial form.
	%
:- mode revert_return == reduced_stack >> partial_stack.



%-----------------------------------------------------------------------------%
%
% Stack insts
%

	% The predicates provided for committing stack changes may be
	% perfomed with all of the various ground and unique insts.
	%
:- inst any_ground
	---> 	ground
	;	unique
	;	mostly_unique.
	
	% This is the 'literal' representation of the enclosed value, without
	% any queued changes to be made to 'commit' it.
	%
:- inst stack_root
	---> 	stack(any_ground).

	% Representation for a stack with queue'd values that will be pushed
	% onto the stack once committed.
:- inst lazy_stack
	---> 	stack(stack_root, ground).

	% This form of the stack 'pretends' that values have been popped off of
	% it, to simplify equality testing. Without this, it would be akward
	% comparing the stack with other stacks with less values.
	%
	% When this variation is 'committed' index values passed to the impure
	% calls are ajusted accordingly.
	%
	% TODO: Work out the shuffling needed to pop values in the middle of a
	% stack, or include a 'remove' call to do so in the impure_stack 
	% typeclass.
	%
:- inst partial_stack
	--->	stack(any_ground, ground).

	% Inst for a stack constructed using pure semantics.
	%
:- inst pure_stack
	--->	stack_root	
	;	lazy_stack
	;	partial_stack.


	% This inst indicates that the mutable state of the stack has been
	% modifed in some manner, and that the value is invalid for Mercury's
	% declarative semantics until the changes are reverted.
	%
:- inst impure_stack 
	---> 	comitted_stack
	;	reduced_stack
	;	impure_stack(ground).


	% Impure stack that has values pushed onto it.
	%
:- inst comitted_stack
	--->	impure_stack(lazy_stack, ground).

	% Impure stack that is unmodified, but any indexed calls to it will be
	% shifted accordingly.
	%
:- inst reduced_stack
	--->	impure_stack(partial_stack, bound(0)).


	% Placing a stack in it's committed form within a pure stack's 
	% recursive structure makes absolutely no sense, and has no 
	% valid meaning.  The same applies for embedding a partial_stack
	% within the middle of a pure_stack's structure.
	%
:- inst invalid_stack
	---> 	stack(impure_stack, ground)
	;	stack(stack(partial_stack, ground))
	;	stack(invalid_stack, ground)
	;	impure_stack(impure_stack).



%-----------------------------------------------------------------------------%

	% This form uses existential types to pass the values, making the
	% stack polymorphic.  It's probably a better idea to use the more 
	% general type.  It's more of an experiment than anything.
	%
:- type stack(T) == stack(stack(T), stack_value(T)).	

	% Value type for stack(T).
	%
:- type stack_value(T)
	---> some [V] ( value(V) => stack(T, V) ).


:- instance pure_stack(stack(T), stack_value(T)).

%-----------------------------------------------------------------------------%
%
%  The Pure stack typeclass
%

% TODO: Talk about this


:- typeclass pure_stack(T) <= impure_stack(T) where [

	% Retreive the int at the top of the stack, 
	% starting at 1 for the bottom.
	% Fails if the stack is empty.
	%
	func top(T)
	mode top(in) is semidet,

	% Retreive the number of values on the stack.
	% Effectively a det version of top.
	%
	func count(T) = int
	mode count(in) = out is det,
	
	% Succeed if the given int is valid.
	%
	pred valid_index(int, T),
	mode valid_index(in, in) is semidet,
	
	% Pop a value off the stack.
	%
	func pop(T) = T,
	mode pop(in) = out is det,
	
	% Pop a number of values off of the stack.
	%
	func pop(int, T) = T
	mode pop(in, in) = out is det,

].

:- typeclass pure_stack(T, V) <= (pure_stack(T), impure_stack(T, V)) where [




	% retreive the value at the given int (Starting at 1 for the bottom).
	%
	func index(int, T) = V
	mode index(in, in) = out is semidet,
	

	% Push a value onto the stack.
	%
	func push(V, T) = T,
	mode push(in, in) = out is det,
	mode push(out, out) = in is det,


].



%-----------------------------------------------------------------------------%
%
%  Impure stack implementation
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


:- typeclass impure_stack(T, V) <= impure_stack(T) where [


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
		impure_stack(T, V), 
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

%:- pred equal_stacks(stack(T, V)::si, stack(T, V)::si) <= pure_stack(T, V)
%	is semidet.

equal_stacks(stack(T),stack(T)).
equal_stacks(stack(S, V), push(V, S)).
equal_stacks(partial_stack(S, N):stack(S, V), pop(N, stack(S, V))).
equal_stacks(pop(N, stack(S, V)), partial_stack(S, N):stack(S, V)).
 


% :- impure pred commit(stack(T, V), T) <= pure_stack(T, V).
% :- 	  mode commit(commit, out) is det.
% :- 	  mode commit(commit, uo) is det.
% :- 	  mode commit(commit, muo) is det.



% :- impure pred revert(T, stack(T, V)) <= pure_stack(T, V).
% :-	  mode revert(in, revert) is det.
% :-	  mode revert(di, revert) is det.
% :-	  mode revert(mdi, revert) is det.


