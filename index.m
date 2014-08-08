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
% TODO
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module index.

:- interface.

:- import_module int.

:- typeclass index(T, Key) where [
	pred index(T, Key, Value),
	mode index(in, in, out) is semidet,
	mode index(in, unused, out) is nondet,
	mode index(in, out, out) is nondet
].

:- typeclass new_index(T, Key) <= index(T, Key) where [
	pred new_index(Index, T, Key, Value) <= index(Index, Key),
	mode new_index(in, out, in, in) is det
].

:- func T ^ index(Key) = Value is semidet <= index(T, Key).

:- func (In ^ index(Key) := Value) = Out 
	<= (index(In, Key), new_index(Out, Key)).

:- func length(T) = int <= index(T,_).
:- pred length(T::in, int::out) is det <= index(T, _).

:- implementation.

:- import_module solutions.

Index ^ index(Key) = Value :- index(Index, Key, Value).

In ^ index(Key) := Value = Out :- new_index(In, Out, Key, Value).

length(T) = L :- 
	aggregate(index(T,_), (func(_, N) = N + 1), 0, L).

length(T, length(T)).

		
		
