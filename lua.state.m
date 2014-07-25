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

:- import_module io.

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
:- func new_state = lua.


	% init(L, !IO).
	% Prepares an existing lua_State for interaction with Mercury
	%
:- pred init(lua::in, io::di, io::uo) is det.
:- impure pred init(lua::in) is det.

	% Verify that Lua is prepared for interaction with Mercury
	%
:- pred ready(lua::in, bool::out, io::di, io::uo) is det.
:- semipure pred ready(lua::in) is semidet.

:- pred get_status(lua::in, status::out, io::di, io::uo) is det.
:- semipure func status(lua) = status.

%-----------------------------------------------------------------------------%
%
% Stack operations.
%


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
:- pred get_top(lua::in, int::out, io::di, io::uo) is det.
:- semipure func top(lua) = int.

	% Set the size of the stack. Any values indexed above the new stack
	% size will be removed from the stack, and any unassigned values at
	% or below the new stack size will be assigned nil values.
	%
:- pred set_top(lua::in, int::in, io::di, io::uo) is det.
:- impure pred 'set_top :='(int::in, lua::in).
	% Ensure that there is space allocated to allow pushing the specified
	% number of variables onto the stack.
	%
:- pred check_stack(int::in, lua::di, lua::uo) is det.


	% Look up a value indexed on the stack.
	%
:- some [T] get_stack(int::in, T::out, lua::in, io::di, io::uo) is det.
:- semipure some [T] func stack(int, lua) = T.

	% Overwrite a value indexed on the stack.
	%
	

	% Look up a global variable.
	%
:- some [T] pred get_global(string::in, T::out, lua::di, lua::uo) is det.

	% Look up a registry variable.
	%
:- some [T] pred get_registry(string::in, T::out, lua::di, lua::uo) is det.



	% Look up a function upvalue.
	%
:- pred some [T] get_upvalue(int::in, T::out, lua::di, lua::uo) is det.






	% Push a value onto the stack.
	%
:- pred push(T::in, lua::di, lua::uo) is det.

	% Pop the specified number of values off of the stack.
	%
:- pred pop(int::in, lua::di, lua::uo) is det.

	% Change a value indexed on the stack.
	%
:- pred set_index(int::in, T::in, lua::di, lua::uo) is det.

	% Change the value of a global variable.
	%
:- pred set_global(string::in, T::in, lua::di, lua::uo) is det.


	% Change the value of a registry variable.
	%
:- pred set_registry(string::in, T::in, lua::di, lua::uo) is det.


	% Change a value of a function upvalue.
	%
:- pred set_upvalue(int::in, T::in, lua::di, lua::uo) is det.


	% Call a function or closure on a unique lua_state.
	% The values on the stack will be used as arguments for the function.
	% The return values will be pushed onto the stack.
	%
:- pred call_function(function::in, lua::di, lua::uo) is det. 

	% call_function(Function, Args, !L).
	%
	% Call a function or closure on a unique lua_state.
	% Args represents the number of values to pop off the stack for the
	% function's arguments. The return values will be pushed onto the end of
	% the stack.
	%
:- pred call_function(function::in, int::in, lua::di, lua::uo) is det. 
