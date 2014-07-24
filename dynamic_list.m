%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2014 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: dynamic_list.m.
% Main author: C4Cypher.
% Stability: low.
% 
% This module presents the dynamic_list type, a polymorphic list that 
% dynamically casts variables, failing on a type mismatch.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type dynamic_list
	--->	some [T] (cons(T, dynamic_list))
	;	[].
	
:- inst cons 
	--->	cons(ground, [])
	;	cons(ground, cons).
	
:- inst empty ---> [].


:- func [ T | args ] = args.
:- mode [ in | in(empty) ] = out(cons) is det.
:- mode [ in | in(cons) ] = out(cons) is det.
:- mode [ out | out ] = in(cons) is semidet.
:- mode [ in | out ] = in(cons) is semidet.
:- mode [ out | out ] = in(empty
