%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2014 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: lookup.m.
% Main author: c4cypher.
% Stability: low.
% 
% Defines a typeclass for performing lookups on types that represent some
% form of key/value binding
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module lookup.

:- interface.


:- typeclass lookup(T, K, V) where [

	pred lookup(K, V, T),
	mode lookup(in, out, in) is semidet,
	mode lookup(out, out, in) is nondet
].

:- func T   ^ K = V <= lookup(T, K, V).
:- mode in  ^ in  = out is semidet.
:- mode in  ^ out = out is nondet.

:- implementation.

T ^ K = V :- lookup(K, V, T).
