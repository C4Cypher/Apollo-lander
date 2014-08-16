%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2014 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: imperative.m.
% Main author: c4cypher.
% Stability: low.
% 
% Typeclass with impure methods for invoking the Lua state.Describe the module.
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module imperative.

:- interface.



% Note: These methods are unsafe without a clear understanding of the workings
% of the Lua C api, and even then, they're still pretty unsafe.
:- typeclass imperative_lua(L) <= lua(L) where [

	% Retreive the value stored by a variable, may invoke metamethods
	impure pred get(var, value, L),	
	mode get(in, out, in) is semidet, 	 	
	mode get(out, out, in) is nondet
	
	% Modify variables in Lua, will not invoke metamethods
	pred rawset(var::in, value::in, L::in) is det,
	
	% Set the value of a variable, may invoke metamethods
	impure pred set(var::in, value::in, L::in) is det,
	
	% Push a value onto the stack
	impure pred push(value::in, L::in) is det,
	
	% Pop a number of values off the stack
	impure pred pop(int::in, L::in) is det
].

% TODO: Abstract representation implementing imperative_lua in pure Mercury


:- instance imperative_lua(lua_state).

:- implementation.



