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

:- module scope.

:- interface.

:- import_module index.


	% Provide an unbound index.
	%
:- typeclass free(T)  where [
	pred free(I::in, T::out) is cc_multi <= index(I, T)
].

:- func free(I) = T <= (free(T), index(I, T)).

:- type scope(T).
:- type scope == scope(univ).

:- instance index(scope(T)) <= index
