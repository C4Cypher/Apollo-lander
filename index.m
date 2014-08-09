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
% An abstract typeclass interface for collections indexed but numeric values. 
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module index.

:- interface.

:- import_module int.

%-----------------------------------------------------------------------------%
%
% Indexes
%
	% index(Id, Value, Index):
	%
	% Represents an Index that uniquely associates Key to value in a one
	% to one relationship. Value should be treated as existential.
	%
:- typeclass index(T) where [
	pred index(int, V, T),
	mode index(in, out, in) is semidet,
	mode index(out, out, in) is nondet
].

	% Index ^ index(Id) = Value
	%
:- func index(int, T) = V is semidet <= index(T).

	% Provide the valid numeric indexes of T.
	%
:- pred index(int::out, T) is nondet. 

	% bounds(Count, First, Last, Index):
	%
	% Count the number of values in an index, providing the first and last
	% indexes.
	%
:- pred bounds(int::out, int::out, int::out, T::in) is det. 

	% Count the number of values in an index.
	%
:- func count(T) = int <= index(T).

	% The lowest index
	%
:- func first(T) = int <= index(T).

	% The highest index
	%
:- func last(T) = int <= index(T).

	% Validate that there are no unbound values between the first and the
	% last values.
	%
:- pred linear(T) is semidet <= index(T).
	
	% linear(!Bounds, Index):
	%
	% Validate that the given index has no unbound values in the given 
	% Bounds.
	%
:- pred linear(int::in, int::in, T::in) is semidet <= index(T).

	% Convert an index into a list of univ values.
	%
:- func univ_list(T) = list(univ) is semidet <= index(T). 

%-----------------------------------------------------------------------------%
%
% Modifying Indexes
%
	
	% new_index(Key, Value, Old, New):
	%
	% Create a new index by assigning a key to a value.
	%
:- typeclass new_index(T) <= index(T) where [
	pred new_index(int, V, T0, T) <= index(T0),
	mode new_index(in, in, in, out) is det
].

	% (Old ^ index(Key) := Value) = New
	%
:- func 'index :='(Key, T0, Value) = T 
	<= (index(T0, Key), new_index(T, Key)).

	% Add a value after the last index.
	%	
:- pred push(V::in, T0::in, T::out) is det <= (index(T0),new_index(T)).
:- func push(V, T0) = T <= (index(T0),new_index(T)).


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module solutions.
:- import_module require.

Index ^ index(Id) = Value :- index(Id, Value, Index).

:- index(I, T) :- index(I, _, T).

bounds(C, F, L, T) :-
	( A = func(I, U@{Count, First0, Last0}) = {Count + 1, First, Last} :-
		( U = {0, 0, 0} -> 
			First = I, Last = I
		; 
			First = (First0 < I -> First0 ; I),
			Last = (Last0 > I -> Last0 ; I)
		)
	),
	(P = pred(I) :- index(I, T)),
	aggregate(P, A, {0, 0, 0}, {C, F, L}).

count(T) = C :- bounds(C, _, _, T).
first(T) = F :- bounds(_, F, _, T).
last(T) = L :- bounds(_, _, L, T).

linear(T) :- 
	bounds(_, F, L, T),	
	linear(F, L, T).
	
linear(F, L, T) :-
	( index(F@L, T)
	; 
		F < L,
		index(F, T),
		linear(F + 1, L, T)
	). 

univ_list(T) = List :-
	expect(linear(T), $module, "Bounds error, index must be continous."), 
	bounds(_, F, L, T),	
	univ_list(F, L, T, List).
	
:- pred univ_list(int::in, int::in, T::in, list(univ)::out) is det.

univ_list(F, L, T, List) :-
		List = [univ(T ^ index(F)) | Next ],
		univ_list(F + 1, L, T, Next)
	;
		unexpected($module, $pred, 
			"Unexpected non value in linear index").

%-----------------------------------------------------------------------------%


In ^ index(Id) := Value = Out :- new_index(In, Out, Id, Value).



		
		
