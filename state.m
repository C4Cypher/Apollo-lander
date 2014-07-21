%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2014 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: state.m
% Main author: C4Cypher.
% Stability: low.
% 
% This library defines types for handling mutable values in a pure matter,
% by allowing one to define semipure queries and reversible impure operations
% that may be performed on said value.  
%  
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module state.

:- interface.

:- import_module int.
:- import_module bool.
:- import_module maybe.

%-----------------------------------------------------------------------------%
%
% The state type.
%

	% Represents a potential state for the given type.
	%
:- type state(T).

	% Capture the state of the given unique value.
	%
:- func state(T) = state(T) <= impure_state(T).
:- mode state(di) = out.
:- mode state(mdi) = out.

	% Check to see that the state is valid.
	%
:- pred valid(state(T)) is semidet.

	% Perform a safe, pure operation on T
	%
:- pred pure(pred(T), state(T)).
:- mode pure((pred(in) is det), in) is det.
:- mode pure((pred(in) is cc_multi), in) is det.
:- mode pure((pred(in) is semidet), in) is semidet.
:- mode pure((pred(in) is cc_nondet), in) is det.
:- mode pure((pred(in) is multi), in) is multi.
:- mode pure((pred(in) is nondet), in) is nondet.

	% Perform a safe, pure operation on T
	%
:- func pure((func(T) = A), state(T)) = A.
:- mode pure((func(in) = out is det), in) = out is det.
:- mode pure((func(in) = outis cc_multi), in) = out is det.
:- mode pure((func(in) = out is semidet), in) = out is semidet.
:- mode pure((func(in) = out is cc_nondet), in) = out is det.
:- mode pure((func(in) = out is multi), in) = out is multi.
:- mode pure((func(in) = out is nondet), in) = out is nondet.

	% Perform a semipure operation on the comitted value of T
	%
:- pred semipure((semipure pred(T)), state(T)).
:- mode semipure(in, (pred(in) is det), in) is det.
:- mode semipure(in, (pred(in) is cc_multi), in) is det.
:- mode semipure(in, (pred(in) is semidet), in) is semidet.
:- mode semipure(in, (pred(in) is cc_nondet), in) is det.
:- mode semipure(in, (pred(in) is multi), in) is multi.
:- mode semipure(in, (pred(in) is nondet), in) is nondet.

	% Perform a safe, pure operation on T
	%
:- func semipure((semipure func(T) = A), state(T)) = A.
:- mode semipure(in, (func(in) = out is det), in) = out is det.
:- mode semipure(in, (func(in) = outis cc_multi), in) = out is det.
:- mode semipure(in, (func(in) = out is semidet), in) = out is semidet.
:- mode semipure(in, (func(in) = out is cc_nondet), in) = out is det.
:- mode semipure(in, (func(in) = out is multi), in) = out is multi.
:- mode semipure(in, (func(in) = out is nondet), in) = out is nondet.

	% Perform a change to a state to produce a different state.
:- func impure(C, state(T)) = state(T) <= impure_call(C,T).

	% Commit the changes made to the state, 
	% producing a unique value with the changes applied.
	% This renders all states created from the value invalid.
	%
:- func commit(state(T)) = T.
:- mode commit(in) = uo.
:- mode commit(in) = muo.









%-----------------------------------------------------------------------------%
%
% Impure state.
%


:- typeclass impure_state(T) <= where [
	impure pred checkpoint(T),
	mode checkpoint(in) is det,
	
	impure pred undo(T),
	mode undo(in) is det
].

:- typeclass impure_call(C, T) <= impure_state(T) where [
	impure pred impure_call(C, T),
	mode impure(in, in) is det,
].


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.


