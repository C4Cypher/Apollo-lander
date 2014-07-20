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
:- import_module bool.
:- import_module maybe.

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
	---> 	some [S] (root(S) => impure_stack(S, T))
	;	pushed(stack(T), T)
	;	reduce(stack(T), int)
	;	comitted(int) 	
	;	reduced(int)
	where equality is equal_stacks.


:- inst ready(S, T) == bound(
	 	pushed(root(S), T)
	;	pushed(ready(S, T), T)
	;	reduce(root(S), int)
).

:- inst unique_ready(S, T) == unique(
	 	pushed(root(S), T)
	;	pushed(unique_ready(T), T)
	;	reduce(root(S), int)
).

:- inst comitted
	--->	comitted(int)
	;	reduced(int).

:- mode commit(S, T) == ready(S, T) >> committed.
:- mode revert(S, T) == comitted >> ready(S, T).

:- mode commit_unique(S, T) == unique_ready(S, T) >> comitted.
:- mode revert_unique(S, T) == comitted >> unique_ready(S, T).

:- mode co(S, T) == commit(S, T).
:- mode re(S, T) == revert(S, T).
:- mode uco(S, T) == commit_unique(S, T).
:- mode ure(S, T) == revert_unique(S, T).


%-----------------------------------------------------------------------------%
%
% Stack operations.
%



	% Construct a stack.
	%
:- pred new_stack(S, stack(T)) <= impure_stack(S, T).
:- mode new_stack(in, out) is det.
:- mode new_stack(di, uo) is det.

:- func to_stack(S) = stack(T) <= impure_stack(S, T).
:- mode to_stack(in) = out is det.
:- mode to_stack(di) = uo is det.


	% Deconstruct a stack.
	%
:- some [S]
	func from_stack(stack(T)) = S => impure_stack(S, T).
:-	mode from_stack(in) = out is det.
:-	mode from_stack(di) = uo is det.

	% Equality test
	%
:- pred equal_stacks(stackS, T), stack(T)).
:- mode equal_stacks(in, in) is semidet.



	% Retreive the index for the value at the top of the stack, starting at 	% 1 for the bottom. This index also represents the number of values on
	% the stack.
	% Returns 0 if the stack is empty.
	% Aborts if the stack is less than 0. (invalid partial stacks).
	%
:- func top(stack(T)) = int.
:- mode top(in) = out is det.
:- mode top(ui) = out is det.

:- pred top(int, stack(T), stack(T)).
:- mode top(out, in, out) is det.
:- mode top(out, di, uo) is det.

	% Test to see if the stack is empty.
	%
:- pred empty(stack(T)::in) is semidet.

	% Returns Yes if the stack is empty.
	%
:- pred empty(bool. stack(T), stack(T)).
:- pred empty(out, in, out) is det.
:- pred empty(out, di, uo) is det.

	
	% Succeed if the given index is valid.
	%
:- pred valid(int:in, stack(T)::in) is semidet.

	% Returns no if the given index is invalid.
	% 
:- pred valid(int, bool, stack(T), stack(T)).
:- mode valid(int, out, in, out) is det.
:- mode valid(in, out, di, uo) is det.

	
	% Pop a value off the stack.
	% Aborts if the stack is empty.
	%
:- pred pop_one(stack(T), stack(T)).
:- mode pop_one(in, out) is det.
:- mode pop_one(di, uo) is det.

:- func pop(stack(T)) = stack(T).
:- mode pop(in) = out is det.
:- mode pop(di) = uo is det.

	% Pop a number of values off of the stack.
	%
:- func pop(int, stack(T)) = stack(T).
:- mode pop(in, in) = out is det.
:- mode pop(in, di) = uo is det.
:- mode pop(out, in) = in is semidet.
:- mode pop(out, in) = out is nondet.

:- pred pop_many(int, stack(T), stack(T)).
:- mode pop_many(in, in, out) is det.
:- mode pop_many(in, di, uo) is det.
:- mode pop_many(out, in, in) is semidet.
:- mode pop_many(out, in, out) is nondet.

	% index(Index, Stack) = Value,
	% top(Stack) = Top, 
	% pop(Top - Index, Stack) = push(Value, pop(Top - (Index + 1), Stack).
	% 
	% Retreive the value at the given int (Starting at 1 for the bottom).
	% Fails if Index is not a valid index for Value on Stack.
	%
:- func index(int, stack(T)) = T <= impure_stack(S, T).
:- mode index(in, in) = out is semidet.
:- mode index(out, out) = in is semidet.
:- mode index(out, in) = out is nondet.

	% maybe_index(Index, Maybe, Stack, Stack),
	% (
	% 	Maybe = yes(Value), Value = index(Index, Stack)
	% ;
	%	Maybe = no, not(_ = index(Index, Stack)
	% ).
	%
	% A more deterministic form of index that returns yes(Value) if
	% Index is a valid index for Value on the Stack.
	% 
:- pred maybe_index(int, maybe(T), stack(T), stack(T)) 
	<= impure_stack(T).
:- mode maybe_index(in, out, in, out) is det.
:- mode maybe_index(out, in, out, in) is semidet.
:- mode maybe_index(out, out, in, out) is nondet.
:- mode maybe_index(in, out, di, uo) is det.

	% Push a value onto the stack.
	%
:- func push(T, stack(T)) = stack(T) <= impure_stack(T).
:- mode push(in, in) = out is det.
:- mode push(out, out) = in is semidet. 
:- mode push(in, di) = uo is det.

:- mode push(out, out) = in is det.
:- mode push(out, uo) = mdi is det.


	% push_value(Value, Stack, push(Value, Stack)).
	% Pred version.
	%
:- pred push_value(T, stack(T), stack(T)) <= impure_stack(T).
:- mode push_value(in, in, out) is det.
:- mode push_value(out, out, in) is semidet.
:- mode push_value(in, di, uo) is det.

	
	% Return the internal representation of the stack with the changes
	% comitted to it.
	%
:- impure some [S]
   pred commit(stack(T), S) <= impure_stack(S, T).
:- mode commit(uco(S, T), out) is det.
:- mode commit(co(S, T), out) is det.


	% Return the stack to it's pure form.
	%
:- impure 
   pred revert(S, stack(T)) <= impure_stack(S, T).
   mode revert(in, re(S, T)) is det.
:- mode revert(in, ure(S, T)) is det.





%-----------------------------------------------------------------------------%
%
% Impure stack typeclass
%

% The impure stack typeclasses present an interface for making impure calls 
% with mutable data types (usually foreign). All of the calls to these
% are exported for use by foreign code.

% For compatability with C, instead of semidet preds, success is indicated
% by int return values, 0 for failure.

:- type boolean == int.



:- typeclass impure_stack(S, T) where [

	% retreive the int for the top value of the stack, fails if empty.
	%
	semipure func stack_top(S) = boolean,
		 mode stack_top(in) = out is det,
	
	% Check to see if a given index is valid.
	%
	semipure func valid_index(int, S) = boolean,
		 mode valid_index(in, in) = out is det,
	
	% Ensure that it is safe to push a number of values onto the stack.
	% Should return 0 if the space for the new values cannot be allocated.
	%
	impure func free_stack(int, S) = boolean,
	       mode free_stack(in, in) = out is det,
	
	% The error to call when impure_free fails.
	%
	semipure pred stack_allocation_error(S),
		 pred stack_alocationn_error(in) is erroneous,
	
	% Pop a number of values off the stack.
	%
	impure pred impure_pop(int, S),
	mode impure_pop(in, in) is det,
	
	% The error to call when one tries to pop off more values than exist.
	%
	semipure pred stack_pop_error(S),
		 pred stack_pop_error(in) is erroneous, 
	
].

:- typeclass impure_stack(T) <= impure_stack(S, U, T).

	% retreive the value at the given int (Starting at 1 for the bottom).
	%
	semipure func get_index(int, S) = T is semidet,
	
	% Push a value onto the stack.
	%
	impure pred impure_push(T, S),
	mode impure_push(in, in) is det,
	
	
].




%-----------------------------------------------------------------------------%
%
% Polymorphic impure stacks
%

	% A polymorphic form of impure_stack, using a higher level type
	% to store the more specific type polymorphically.
	%
:- typeclass impure_stack(S, U, T) <= (
		(S -> U),  
		impure_stack(S, U)
) where [
	
	% Cast the more specific type to the more general type.
	%
	func upcast(T) = U,
	mode upcast(in) = out is det,
	
	% Cast the more general type to the more specific type, fail if
	% T does not match the type of the value stored by U
	func downcast(U) = T,
	mode downcast(in) = out is semidet

].


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

% :- pred new_stack(S, stack(T)) <= impure_stack(S, T).
% :- mode new_stack(in, out) is det.
% :- mode new_stack(di, uo) is det.

new_stack(S, to_stack(S)). 

% :- func to_stack(S) = stack(T) <= impure_stack(S, T).
% :- mode to_stack(in) = out is det.
% :- mode to_stack(di) = uo is det.

to_stack(S) = 'new root'(S).


% :- some [S]
% 	func from_stack(stack(T)) = S => impure_stack(S, T).
% :-	mode from_stack(in) = out is det.
% :-	mode from_stack(di) = uo is det.

from_stack(root(S)) = S.
from_stack(pushed(S, _)) = from_stack(S).
from_stack(reduced(S, _)) = from_stack(S).

% :- pred equal_stacks(stackS, T), stack(T)).
% :- mode equal_stacks(in, in) is semidet.

equal_stacks(S1), S2) :- T1@top(S1) = T2@top(S2),  
	( S1@root(S) = S2@root(S) 
	; index(Index, S1) = index(Index, S2) ).

% :- func top(stack(T)) = int.
% :- mode top(in) = out is det.
% :- mode top(ui) = out is det.

top(root(S)) = stack_top(S).
top(pushed(S, _)) = top(S) + 1.
top(reduce(S, N)) = T@(top(S) - N) :- not (T < 0, 
	unexpected($module, $pred, "Reduced stack resulted in stack carrying "
	++ T ++ " values.")).  

% :- pred top(int, stack(T), stack(T)).
% :- mode top(out, in, out) is det.
% :- mode top(out, di, uo) is det.

top(stack_top(S), root(S), 'new root'(S)).
top(T, pushed(!.S, V), pushed(!:S, V)) :- top(T - 1, !S).
top(T, reduce(!.S, N), reduce(!:S, N) = top(T - N,  !S), not(T < 0,
	unexpected($module, $pred, "Reduced stack resulted in stack carrying "
	++ T ++ " values.")).  

% :- pred empty(stack(T)::in) is semidet.

empty(S) :- top(S) = 0.

% :- pred empty(bool. stack(T), stack(T)).
% :- pred empty(out, in, out) is det.
% :- pred empty(out, di, uo) is det.

empty(B, S, S) :- empty(S), B = yes ; B = no.

% :- pred valid_index(int:in, stack(T)::in) is semidet.

valid(Index, S) :- Index > 0 , Index =< top(S).

% :- pred valid(int, bool, stack(T), stack(T)).
% :- mode valid(int, out, in, out) is det.
% :- mode valid(in, out, di, uo) is det.

valid(Index, B, !S) :- B = yes, Index > 0 , Index =< Top, top(Top, !S).

% :- pred pop_one(stack(T), stack(T)).
% :- mode pop_one(in, out) is det.
% :- mode pop_one(di, uo) is det.

pop_one(S, pop(S)).

% :- func pop(stack(T)) = stack(T).
% :- mode pop(in) = out is det.
% :- mode pop(di) = uo is det.

pop(S) = pop(1, S).


% :- func pop(int, stack(T)) = stack(T).
% :- mode pop(in, in) = out is det.
% :- mode pop(in, di) = uo is det.
% :- mode pop(out, in) = in is semidet.
% :- mode pop(out, in) = out is nondet.

pop(N, root(S)) = pop(N, reduce(root(S), 0)).
pop(N, reduce(!.0, R)) = reduce(!:S, R + N) :- top(T, !S), not(T - R - N < 0, 
	unexpected($module, $pred, 
	"Invalid stack operation: attempted to remove " ++ R + N - T ++ 
	" more values than the stack contained.")).


% :- pred pop_many(int, stack(T), stack(T)).
% :- mode pop_many(in, in, out) is det.
% :- mode pop_many(in, di, uo) is det.
% :- mode pop_many(out, in, in) is semidet.
% :- mode pop_many(out, in, out) is nondet.

% :- func index(int, stack(T)) = T <= impure_stack(S, T).
% :- mode index(in, in) = out is semidet.
% :- mode index(out, out) = in is semidet.
% :- mode index(out, in) = out is nondet.

% :- pred maybe_index(int, maybe(T), stack(T), stack(T)) 
% 	<= impure_stack(S, T).
% :- mode maybe_index(in, out, in, out) is det.
% :- mode maybe_index(out, in, out, in) is semidet.
% :- mode maybe_index(out, out, in, out) is nondet.
% :- mode maybe_index(in, out, di, uo) is det.

% :- func push(T, stack(T)) = stack(T) <= impure_stack(S, T).
% :- mode push(in, in) = out is det.
% :- mode push(out, out) = in is semidet. 
% :- mode push(in, di) = uo is det.

% :- mode push(out, out) = in is det.
% :- mode push(out, uo) = mdi is det.

% :- pred push_value(T, stack(T), stack(T)) <= impure_stack(S, T).
% :- mode push_value(in, in, out) is det.
% :- mode push_value(out, out, in) is semidet.
% :- mode push_value(in, di, uo) is det.


% :- impure some [S]
%  pred commit(stack(T), S) <= impure_stack(S, T).
% :- mode commit(uco(S, T), out) is det.
% :- mode commit(co(S, T), out) is det.


% :- impure 
%    pred revert(S, stack(T)) <= impure_stack(S, T).
%    mode revert(in, re(S, T)) is det.
% :- mode revert(in, ure(S, T)) is det.

% :- type boolean == int.

:- instance impure_stack(stack(T)) where [

% 	semipure func stack_top(S) = boolean,
% 		 mode stack_top(in) = out is det,

% 	semipure func valid_index(int, S) = boolean,
% 		 mode valid_index(in, in) = out is det,

% 	impure func free_stack(int, S) = boolean,
% 	       mode free_stack(in, in) = out is det,

% 	semipure pred stack_allocation_error(S),
% 		 pred stack_alocationn_error(in) is erroneous,
% 	

% 	impure pred impure_pop(int, S),
% 	mode impure_pop(in, in) is det,

% 	semipure pred stack_pop_error(S),
% 		 pred stack_pop_error(in) is erroneous, 


% 	semipure func get_index(int, S) = T is semidet,


% 	impure pred impure_push(T, S),
% 	mode impure_push(in, in) is det,

].

