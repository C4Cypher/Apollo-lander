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
:- semipure some [T] pred get_stack(lua::in, index::in, T::out)

	% Push a global variable onto the stack.
	%
:- semipure some [T] pred get_global(lua::in, string::in, T::out) is det.

	% Set a global variable from the top of the stack.
	%
:- impure pred set_global(lua::in, string::in) is det.


	% Push a registry variable onto the stack.
	%
:- semipure some [T] pred get_registry(lua::in, string::in, T::out) is det.

	% Set a registry variable from the top of the stack.
	%
:- impure pred set_registry(lua::in, string::in) is det.


	% Push a function upvalue onto the stack. Fail if the upvalue is 
	% not valid.
	%
:- semipure some [T] pred get_upvalue(lua::in, int::in, T::out) is semidet.

	% Set a function upvalue from the top of the stack. Fail if the upvalue
	% is not valid.
	%
:- impure pred set_upvalue(lua::in, int::in, T::in) is semidet.


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

:- pragma foreign_proc("C",  get_global(L::in, Name::in, T::out),
	[promise_semipure, will_not_call_mercury],
"
	lua_getglobal(L, Name);	
").

:- pragma foreign_proc("C",  set_global(L::in, Name::in),
	[will_not_call_mercury],
"
	lua_setglobal(L, Name)
").

:- pragma foreign_proc("C",  get_registry(L::in, Key::in),
	[promise_semipure, will_not_call_mercury],
"
	luaMR_getregistry(L, Key);
").


:- pragma foreign_proc("C",  set_registry(L::in, Key::in),
	[will_not_call_mercury],
"
	luaMR_setregistry(L, Key);
").

:- pragma foreign_proc("C",  get_upvalue(L::in, Id::in, T::out),
	[promise_semipure, will_not_call_mercury],
"
	if(luaMR_valid_upvalue(L, Id)) {
		luaMR_push_upvalue(L, Id);
		SUCCESS_INDICATOR = 1;
	}
	else
		SUCCESS_INDICATOR = 0;
").


:- pragma foreign_proc("C",  set_upvalue(L::in, Key::in, T::in),
	[will_not_call_mercury],
"
	if(luaMR_valid_upvalue(L, Id)) {
		luaMR_push_upvalue(L, Id);
		SUCCESS_INDICATOR = 1;
	}
	else
		SUCCESS_INDICATOR = 0;
").

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


