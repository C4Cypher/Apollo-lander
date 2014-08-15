%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2014 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: assoc.m.
% Main author: c4cypher.
% Stability: low.
% 
% Defines a typeclass providing a generic interface for interacting with
% associative data structures in a pure, declarative manner.
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module assoc.

:- interface.

%-----------------------------------------------------------------------------%
%
% Lookup
%

	% Examine the contents of an associative data structure
	%
:- typeclass lookup(T, K, V) where [

	pred lookup(K, V, T),
	mode lookup(in, out, in) is semidet,
	mode lookup(out, out, in) is nondet
].


:- func T  ^ K = V <= lookup(T, K, V).
:- mode in ^ in  = out is semidet.
:- mode in ^ out = out is nondet.

%-----------------------------------------------------------------------------%
%
% Assign
%

	% Assign values to keys, producing new data structures.
	%
:- typeclass assign(T1, T2, K, V) <= lookup(T1, K, V) where

	pred assign(K, V, T1, T2),
	mode assign(in, in, in, out) is det,
	mode assign(out, out, out, in) is nondet,
].

:- typeclass assign(T, K, V) <= assign(T, T, K, V) where [ ].

:- func (T0  ^ K   := V  ) = T <= assign(T0, T, K, V).
:- mode (in  ^ in  := in ) = out is det.
:- mode (out ^ out := out) = in  is nondet.


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
	
:- implementation.

T ^ K = V :- lookup(K, V, T).

(T0 ^ K := V) = T :- assign(K, V, T0, T).
