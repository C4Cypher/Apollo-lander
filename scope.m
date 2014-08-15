%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2014 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: property.m.
% Main author: c4cypher.
% Stability: low.
% 
% Defines a set of types and typeclasses for handling associative types
% in a manner that imitates dynamic variable binding with lexical scope.
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module scope.

:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%
%
% Getting values from scope
%

	% Examine the contents of an associative data structure
	%
:- typeclass scope(T, K, V) where [

	pred get(K, V, T),
	mode get(in, out, in) is semidet,
	mode get(out, out, in) is nondet
].


:- func T  ^ K = V <= scope(T, K, V).
:- mode in ^ in  = out is semidet.
:- mode in ^ out = out is nondet.

%-----------------------------------------------------------------------------%
%
% Modifying scope
%

	% Update key value assignments, producing new data structures with the
	% assignment added or overwritten.
	%
:- typeclass immutable_scope(T, K, V) <= scope(T, K, V) where [

	pred set(K::in, V::in, T0::in, T::out) is det <= scope(T0, K, V),
	pred remove(K::in, T0::in, T::out) is det <= scope(T0, K, V)
].

:- typeclass mutable_scope(T, K, V) <= scope(T, K, V) where [
	pred set(K::in, V::in, T::in, io::di, io::uo) is det
		<= scope(T, K, V),
	pred remove(K::in, T::in, io::di, io::uo) is det
].

:- func (T ^ K := V) = T <= assign(T, K, V).
:- mode (in ^ in := in) = out is det.
:- mode (out ^ out := out) = in is nondet.



:- type scope(T).





	
	
