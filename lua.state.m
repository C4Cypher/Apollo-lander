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

:- module state.

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
:- type lua == lua_state.

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
	;	yield
	;	runtime_error
	;	syntax_error
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
% to the Lua Refrence Manual, and the examples provided at the Lua User's Wiki.

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
	
	% Look up the type of a value indexed on the stack.
	%
:- semipure pred get_type(lua::in, index::in, lua_type::out) is det.
 
	% Look up a value indexed on the stack.
	%
:- semipure some [T] pred get_stack(lua::in, index::in, T::out) is det.

:- semipure some [T] func stack(lua, index) = T.


	% Push a global variable onto the stack.
	%
:- semipure pred push_global(lua::in, string::in) is det.

	% Retreive a global variable.
	%
:- semipure some [T] pred get_global(lua::in, string::in, T::out) is det.

:- semipure some [T] func global(lua, string) = T.

	% Set a global variable from the top of the stack.
	%
:- impure pred pop_global(lua::in, string::in) is det.

	% Set a global variable.
	%
:- impure pred set_global(lua::in, string::in, T::in) is det.

	% Push a registry value onto the stack.
	%
:- semipure pred push_registry(lua::in, string::in, T::out) is det.

	% Retreive a registry value.
	%
:- semipure some [T] pred get_registry(lua::in, string::in, T::out) is det.

:- semipure some [T] func registry(lua, string) = T.


	% Set a registry valua from the top of the stack.
	%
:- impure pred pop_registry(lua::in, string::in) is det.

	% Set a registry value.
	% 
:- impure pred set_registry(lua::in, string::in) is det.

	% Push a function upvalue onto the stack. 
	%
:- semipure pred push_upvalue(lua::in, int::in) is det.

	% Retreive a function upvalue.
	%
:- semipure some [T] pred get_upvalue(lua::in, int::in, T::out) is det.

:- semipure some [T] func upvalue(lua, int) = T.

	% Set a function upvalue from the top of the stack. 
	%
:- impure pred pop_upvalue(lua::in, int::in) is det.

	% Set a function upvalue. 
	%
:- impure pred set_upvalue(lua::in, int::in, T::in) is det.

	% Push a value onto the stack.
	%
:- impure pred push(lua::in, T::in) is det.

	% Pop the specified number of values off of the stack.
	%
:- impure pred pop(lua::in, int::in) is det.



	% Call a function, specifying the number of arguments pushed onto the 
	% stack above it. The function and argumetns are removed from the stack.
	% If an error is encountered, the provided pred is called with the
	% string error message. The function's return values will be pushed onto
	% the stack if no error is encountered.
	%
:- impure pred pcall(lua, int, pred(string).
:- mode pcall(in, in, pred(in) is det) is det.
:- mode pcall(in, in, pred(in) is erroneous) is det. 







%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%


:- implementation.

:- func return_nil = nil.

return_nil = nil.

:- pragma foreign_export("C", return_nil = out, "luaMR_nil").


:- pragma foreign_proc("C", new_state = L::out, 
	[promise_pure, will_not_call_mercury],
"
	L = luaL_newstate();
").

:- pragma foreign_proc("C", get_status(L::in, Status::out), 
	[promise_semipure, will_not_call_mercury],
"
	Status = lua_status(L);
").

:- pragma foreign_enum("C", lua_status/0, 
[
	ready - 0,
	yield - "LUA_YIELD",
	runtime_error - "LUA_ERRRUN",
	syntax_error - "LUA_ERRSYNTAX",
	memory_error - "LUA_ERRMEM",
	unhandled_error - "LUA_ERRERR"
] ).


:- pragma foreign_proc("C", get_top(L::in, Index::out),
	[promise_semipure, will_not_call_mercury],
"
	Index = lua_gettop(L);
").

	
	%
:- pragma foreign_proc("C",  set_top(L::in, Index::in),
	[will_not_call_mercury],
"
	lua_settop(L, Index);
").

:- pragma foreign_proc("C",  check_stack(L::in, Free::in),
	[will_not_call_mercury],
"
	lua_checkstack(L, Free);
").

:- pragma foreign_proc("C",  get_type(L::in, Index::in, Type::out),
	[promise_semipure, will_not_call_mercury],
"
	Type = lua_type(L, Index);
").

:- pragma foreign_proc("C",  get_stack(L::in, Index::in, T::out),
	[promise_semipure, will_not_call_mercury],
"
	switch(lua_type(L, Index)) {
		case LUA_TNIL:
			T = luaMR_nil();
			break;
		case LUA_TBOOLEAN:
			if (lua_toboolean(L, Index))
				T = MR_YES;
			else
				T = MR_NO;
			break;
		case LUA_TLIGHTUSERDATA:
			T = lua_tolightuserdata(L, Index);
			break;
		case LUA_TNUMBER:
			T = lua_tonumber(L, Index);
			break;
		case LUA_TSTRING:
			T = lua_tostring(L, Index);
			break;
		case LUA_TTABLE:
		case LUA_TFUNCTION:
			T = luaMR_new_ref(L, Index);
			break;
		case LUA_TUSERDATA:
			T = lua_touserdata(L, Index);
			break;
		case LUA_TTHREAD;
			T = lua_tothread(L, Index);
			break;
		case LUA_TNONE:
		default:
			MR_fatal_error(
			""lua.state.get_stack/3: Invalid value on the stack"");
	} /* switch */
").

stack(L, Id) = T :- get_stack(L, Id, T).

%	none - "LUA_TNONE",
%	nil - "LUA_TNIL",
%	boolean - "LUA_TBOOLEAN",
%	lightuserdata - "LUA_TLIGHTUSERDATA",
%	number - "LUA_TNUMBER",
%	string - "LUA_TSTRING",
%	table - "LUA_TTABLE",
%	function - "LUA_TFUNCTION",
%	userdata - "LUA_TUSERDATA",
%	thread - "LUA_TTHREAD"

:- pragma foreign_proc("C",  push_global(L::in, Name::in),
	[promise_semipure, will_not_call_mercury],
"
	lua_getglobal(L, Name);	
").

get_global(L, Name, T) :- 
	promise_semipure (
		impure push_global(L, Name),
		semipure get_stack(L, -1),
		impure pop(L, 1)
	).
	
global(L, Name) = T :- get_global(L, Name, T).

:- pragma foreign_proc("C",  pop_global(L::in, Name::in),
	[will_not_call_mercury],
"
	lua_setglobal(L, Name)
").

set_global(L, Name, T) :-
	impure push(L, T),
	impure pop_global(L, Name).

:- pragma foreign_proc("C",  push_registry(L::in, Key::in),
	[promise_semipure, will_not_call_mercury],
"
	luaMR_getregistry(L, Key);
").

get_registry(L, Key, T) :-
	promise_semipure (
		impure push_registry(L, Name),
		semipure get_stack(L, -1),
		impure pop(L, 1)
	).

registry(L, Key) = T :- get_registry(L, Key, T).

:- pragma foreign_proc("C",  pop_registry(L::in, Key::in),
	[will_not_call_mercury],
"
	luaMR_setregistry(L, Key);
").

set_registry(L, Name, T) :-
	impure push(L, T),
	impure pop_registry(L, Name).
	

:- pragma foreign_proc("C",  push_upvalue(L::in, Id::in),
	[promise_semipure, will_not_call_mercury],
"
		luaMR_getupvalue(L, Id);
").

get_upvalue(L, Id, T) :-
	promise_semipure (
		impure push_upvalue(L, Id),
		semipure get_stack(L, -1),
		impure pop(L, 1)
	).

upvalue(L, Id) = T :- get_upvalue(L, Id, T).


:- pragma foreign_proc("C",  pop_upvalue(L::in, Key::in),
	[will_not_call_mercury],
"
	luaMR_setupvalue(L, Id);
").

set_upvalue(L, Name, T) :-
	impure push(L, T),
	impure pop_upvalue(L, Name).

:- pragma foreign_proc("C",  push(L::in, T::in),
	[will_not_call_mercury],
"
MR_fatal_error("lua.state.push/2 has not yet been implemented. Sorry.");
").

:- pragma foreign_proc("C",  pop(L::in, Num::in),
	[will_not_call_mercury],
"
	lua_pop(L, Num);
").


:- pragma foreign_proc("C",  call(L::in, Args::in, Err::in) = Status,
	[will_not_call_mercury],
"
	/* TODO */
MR_fatal_error("lua.state.pcall/3 has not yet been implemented. Sorry.");
").


