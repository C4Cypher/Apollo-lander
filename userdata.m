%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2014 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: userdata.m.
% Main author: C4Cypher.
% Stability: low.
% 
% This file provides Lua metamethods for Mercury variables passed to Lua.
% It also provides the userdata typeclass, allowing instances to extend the
% behavior of those metamethods when they are called Lua.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module userdata.

:- interface.

:- type metamethod
	--->	add	%	+
	;	sub	%	-
	;	mul	%	*
	;	div	%	/
	; 	mod	%	%
	;	pow	%	^
	;	unm	%	-
	;	concat	%	..
	;	len	%	#
	;	eq	%	==
	;	lt	%	<
	;	le	%	<=
	;	index	%	u[k]
	;	newindex%	u[k] = v
	;	call	%	u(...)
	;	gc.


:- typeclass userdata(T) where [
	pred metamethod(metamethod, T, pred(lua_state, list(var)),
	mode metamethod(in, in, out(pred(in, out) is det)) is nondet
].


:- implementation.



