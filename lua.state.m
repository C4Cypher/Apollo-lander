%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2014 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: lua.state.m.
% Main author: C4Cypher.
% Stability: low.
% 
% This file provides low level access to calls for manipulating the Lua state.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module lua.state.

:- interface.


%-----------------------------------------------------------------------------%
%
% The Lua state
%

	% The lua type is a refrence to the Lua state, also known as the
	% Lua Virtual Machine.  This type is defined in lua.h as
	% the C type "lua_State *". Note that as a convention borrowed from 
	% the C API, operations that query or manipulate the Lua state will
	% use the variable term 'L' to refer to the Lua state.
	%
:- type lua.

% WARNING! Refrences to Lua types (tables, functions, userdata) derived
% from one lua_state are NOT compatible with other seperately created
% lua_states. The only exception to this is lua_states created as threads.
% lua_threads may freely pass variables to or from their parent state and
% sibling threads.

	% Create a fresh, new , initialized lua_state.
	%
:- pred new_state(lua::out).



	% Return the Lua state's current status.
	%
:- semipure pred get_status(lua::in, status::out). 


:- type status
	---> 	ready
	;	yeild
	;	runtime_error
	;	memory_error
	;	unhandled_error.

%-----------------------------------------------------------------------------%
%
% Stack operations.
%

:- type index == int.

% Each function call is provided with a local stack which function arguments
% are pushed onto before the call.  The function call returns an integer
% and Lua uses that number to determine the number of return values to take
% off the top of the stack (from the bottom up).  In both cases the first 
% argument is pushed first, with the last argument on the top of the stack.
% 
% Values on the stack can be refrenced by integer index values. Positive 
% integers refrence values from the bottom of the stack, starting at one,
% while negative integers refrences the stack from the top down (-1 referring
% to the value at the top of the stack).

% Due to the fact that different versions of Lua handle the global environment
% and the registry in different ways, for the sake of compatability, this
% library will not permit the explicit use of pseudo-indexes.  Instead, 
% seperate access predicates have been provided in the place of pseudo-indexes.

% Warning: Lua employs minimal error checking when performing low level
% stack operations. It trusts that code directly manipulating the stack
% will avoid using invalid stack refrences or stack overflows through the use
% of top, get_top, set_top and check_stack.  For more information, refer
% to the Lua Refrence Manual, and the examples provided at the Lua Uer's Wiki.

	% Retreive the index for the top value on the stack.
	% Also represents the number of values on the stack.
	%
:- semipure pred get_top(lua::in, index::out) is det.

	% Set the size of the stack. Any values indexed above the new stack
	% size will be removed from the stack, and any unassigned values at
	% or below the new stack size will be assigned nil values. Aborts
	% if the new size is less than zero. 
	%
:- impure pred set_top(lua::in, index::in) is det.

	% Ensure that there is space allocated to allow pushing the specified
	% number of variables onto the stack.
	%
:- impure pred check_stack(lua::in, int::in) is det.


	% Look up a value indexed on the stack.
	%
:- semipure some [T] pred get_stack(lua, index, T).
:- mode stack(in, in, out) is det.
:- mode stack(in, out, out) is nondet.

	% Look up a global variable.
	%
:- semipure some [T] pred get_global(lua::in, string::in, T::out) is det.

	% Modify a global variable.
	%
:- impure pred set_global(lua::in, string::in, T::in) is det.


	% Look up a registry variable.
	%
:- semipure some [T] pred get_registry(lua::in, string::in, T::out) is det.

	% Modify a global variable.
	%
:- impure pred set_registry(lua::in, string::in, T::in) is det.


	% Look up a function upvalue. Fail if the upvalue is not valid.
	%
:- semipure some [T] pred get_upvalue(lua::in, int::in, T::out) is semidet.

	% Modify a global variable. Fail if the upvalue is not valid.
	%
:- impure pred set_registry(lua::in, int::in, T::in) is semidet.


	% Push a value onto the stack.
	%
:- impure pred push(lua::in, T::in) is det.

	% Pop the specified number of values off of the stack.
	%
:- impure pred pop(lua::in, int::in) is det.



	% Call a function and remove it from the stack. The values on the 
	% stack after the function will be removed and passed as arguments. 
	% The function's return values will be pushed onto the stack.
	%
:- impure pred call(lua::in, index::in) is det. 



%-----------------------------------------------------------------------------%
%
% Refrences
%

	% Create a refrence from a value on the stack.
	%
:- impure pred new_ref(lua::in, index::in, int::out) is det.

	% Push a refrence onto the stack.
:- impure pred push_ref(lua::in, int::in) is det.

	% Look up a refrence. Fail if it doesn't exist.
	%
:- semipure some [T] pred get_ref(lua::in, int::in, T::out) is det.


	% Remove a refrence.
	%
:- impure unref(lua::in, int::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%


:- implementation.

:- lua == lua.lua_state.
