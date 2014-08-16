%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2014 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: state.m.
% Main author: C4Cypher.
% Stability: low.
% 
% This file provides access to some of the impure, lower level calls of the
% Lua API for manipulating the Lua state.
%
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
%
% Due to the fact that different versions of Lua handle the global environment
% and the registry in different ways, for the sake of compatability, this
% library will not permit the explicit use of pseudo-indexes.  Instead, 
% seperate access predicates have been provided in the place of pseudo-indexes.
%
% Warning: Lua employs minimal error checking when performing low level
% stack operations. It trusts that code directly manipulating the stack
% will avoid using invalid stack refrences or stack overflows through the use
% of top, get_top, set_top and check_stack.  For more information, refer
% to the Lua Refrence Manual, and the examples provided at the Lua User's Wiki.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module state.

:- interface.

:- import_module stream.

% Note: These methods are unsafe without a clear understanding of the workings
% of the Lua C api, and even then, they're still pretty unsafe.



% TODO: Abstract representation implementing imperative_lua in pure Mercury


%-----------------------------------------------------------------------------%
%
% Stack Manipulation
%
	
	% The index at the top of the stack.
:- semipure func lua_gettop(lua) = int.
:- impure pred 	lua_settop(int::in, lua::in) is det.

	% Valid indexes on the stack
:- semipure indexes(int::out, lua::in) is nondet.

:- interface.

:- pragma foreign_proc("C", lua_gettop(lua::in, Index::out),
	[promise_semipure, will_not_call_mercury],
	"Index = lua_gettop(L); ").
	
:- pragma foreign_proc("C",  set_top(Index::in, L::in),
	[will_not_call_mercury],
	"lua_settop(L, Index);").

indexes(I, L) :-
		semipure lua_gettop(L, I)
	;
		get_indexes(L, I0),
		I = I0 - 1,
		I > 0.
:- interface.
	
	% Allocate free space on the stack if needed, fail if it cannot
:- semipure pred lua_checkstack(int::in, lua::in) is semidet.

	% Directly push values from a different stack index
:- impure pred 	lua_pushvalue(int::in, lua::in) is det.

	% Pop 
:- impure pred 	lua_pop(int::in, lua::in) is det.

% Note: Use of lua_remove and lua_insert is highly discouraged when used with 
% this library, given that said operations impurely re-arrange the Lua stack
% in a manner that ignores restrictions that Mercury needs to interact with
% it purely.

:- implementation.

:- pragma foreign_proc("C",  lua_checkstack(lua::in, Free::in),
	[will_not_call_mercury, promise_semipure], "lua_checkstack(L, Free);").

- pragma foreign_proc("C",  lua_pushvalue(I::in, L::in),
	[will_not_call_mercury], "lua_pushvalue(L, I);").

:- pragma foreign_proc("C",  pop(lua::in, Num::in),
	[will_not_call_mercury], "lua_pop(L, Num);").
:- interface.


%-----------------------------------------------------------------------------%
%
% Accessing and manipulating variables 
%

	
	% The Lua type of a value on the stack
	%
:- semipure func lua_gettype(value, lua) = lua_type.
	
	% Access Lua tables without invoking metamethods
	%
:- impure pred lua_rawget(index::in, lua::in) is det.
:- impure pred lua_rawset(index::in, lua::in) is det.

	% Access Lua tables, if Raw is yes, metamethod invocations are avoided,
	% but an error is thrown if Table is not actually a table.
	%
:- impure pred lua_gettable(index::in, lua::in) is det.
:- impure pred lua_settable(index::in, lua::in) is det.
	
	% Access metatables, may cause undefined behavior if used on types
	% that do not have metatables.
	%
:- impure pred lua_getmetatable(index::in, lua::in) is det.
:- impure pred lua_setmetatable(index::in, lua::in) is det.

:- implementation.

:- pragma foreign_proc("C",  get_type(Index::in, L::in) = (Type::out)), 
	[promise_semipure, will_not_call_mercury],
	"Type = lua_type(L, Index);").

:- pragma foreign_proc("C", lua_rawget(I::in, L::in), 
	[will_not_call_mercury], "lua_rawget(L, I);").
	 
:- pragma foreign_proc("C", lua_rawset(I::in, L::in), 
	[will_not_call_mercury], "lua_rawset(L, I);"). 

:- pragma foreign_proc("C", lua_gettable(I::in, L::in), 
	[will_not_call_mercury], "lua_gettable(L, I);"). 
:- pragma foreign_proc("C", lua_settable(I::in, L::in), 
	[will_not_call_mercury], "lua_settable(L, I);"). 
	
:- pragma foreign_proc("C", lua_getmetatable(I::in, L::in), 
	[will_not_call_mercury], "lua_getmetatable(L, I);"). 

:- pragma foreign_proc("C", lua_setmetatable(I::in, L::in), 
	[will_not_call_mercury], "lua_setmetatable(L, I);"). 

:- interface.

%-----------------------------------------------------------------------------%
%
% Function constructors, deconstructors, and calls 
%

/*
	% Load a function from a string.
:- impure pred lua_loadstring(string::in, lua::in) is det.

	% Dump a function as a compiled chunk to a string writer.
:- impure pred func lua_dump(var, lua) = string.

	% call a function
:- impure func lua_call(int, int, lua) = values.

	
	% call a function with an error handler.
	%
:- impure func pcall(var, values, var, lua) = values.

	% cpcall(CFunc, LUdataIn, L) = LUdataOut
	%
	% Protected C call in Lua, passing a pointer (or MR_Word)
	% as the only argument.  
	% 
:- impure func cpcall(c_function, c_pointer, lua) = c_pointer.

	% c_clousure(CFunc, Upvalues, L) = Function
	%
	% Create a Lua function, associating upvalues with it
	% upvalues may be omitted to just push the C function.
	%
:- func lua_pushcclosure(c_function, vars, L) = var.
:- func lua_cclosure(c_function, L) = var. % :- F = c_closure(CFunc, [], L).

*/

%-----------------------------------------------------------------------------%
%
% Utilites for the concrete Lua state.
%
	% Create a fresh, new , initialized lua_state.
	%
:- pred lua_newstate(lua_state::out).

	% Destroy a Lua_state
	%
:- impure pred lua_close(lua_state::in).	


	% Return the Lua state's current status.
	%
:- semipure pred lua_status(lua_state::in, status::out). 


:- type status
	---> 	ready
	;	yield
	;	runtime_error
	;	syntax_error
	;	memory_error
	;	unhandled_error.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.	
	
:- func return_nil = nil.

return_nil = nil.

:- pragma foreign_export("C", return_nil = out, "luaMR_nil").


:- pragma foreign_proc("C", lua_newstate = (L::out), 
	[promise_pure, will_not_call_mercury], 	"L = luaL_newstate();").
	
:- pragma foreign_proc("C", lua_close(L::in), 
	[promise_pure, will_not_call_mercury], "lua_close(L);").

:- pragma foreign_proc("C", lua_status(lua::in, Status::out), 
	[promise_semipure, will_not_call_mercury], "Status = lua_status(L);").

:- pragma foreign_enum("C", status/0, [
	ready - "0",
	yield - "LUA_YIELD",
	runtime_error - "LUA_ERRRUN",
	syntax_error - "LUA_ERRSYNTAX",
	memory_error - "LUA_ERRMEM",
	unhandled_error - "LUA_ERRERR"
] ).

:- interface.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Value passing 
%

:- semipure pred lua_isnumber(index::in, L::in) is semidet.
:- semipure pred lua_isnil(index::in, L::in) is semidet.
:- semipure pred lua_isuserdata(index::in, L::in) is semidet.
:- semipure pred lua_isinteger(index::in, L::in) is semidet.
:- semipure pred lua_islightuserdata(c_pointer::in, L::in) is semidet.
:- semipure pred lua_isstring(index::in, L::in) is semidet.
:- semipure pred lua_isboolean(index::in, L::in) is semidet.
:- semipure pred lua_isthread(index::in, L::in) is semidet.
:- semipure pred lua_isfunction(index::in, L::in) is semidet.
:- semipure pred lua_iscfunction(index::in, L::in) is semidet.

:- semipure pred lua_tonumber(index::in, float::out, L::in) is det.
%:- semipure pred lua_touserdata(index::in, univ::out, L::in) is det.
:- semipure pred lua_tointeger(index::in, int::out, L::in) is det.
:- semipure pred lua_tolightuserdata(index::in, c_pointer::out, L::in) is det.
:- semipure pred lua_tostring(index::in, string::out, L::in) is det.
:- semipure pred lua_toboolean(index::in, boolua::out, L::in) is det.
:- semipure pred lua_tothread(index::in, index::out, L::in) is det.
:- semipure pred lua_tocfunction(index::in, c_function::out, L::in) is det.

:- impure pred lua_pushnumber(float::in, L::in) is det.
:- impure pred lua_pushnil(L::in) is det.
%:- impure pred lua_pushuserdata(univ::in, L::in) is det.
:- impure pred lua_pushinteger(int::in, L::in) is det.
:- impure pred lua_pushlightuserdata(c_pointer::in) is det.
:- impure pred lua_pushstring(string::in, L::in) is det.
:- impure pred lua_pushboolean(bool::in, , L::in) is det.
:- impure pred lua_isfunction(index::in, L::in) is det.
:- impure pred lua_iscfunction(index::in, L::in) is det.


%-----------------------------------------------------------------------------%

:- implementation.


:- pragma foreign_proc("C", lua_isnumber(Index::in, L::in),
	[promise_semipure, will_not_call_mercury],
	" SUCCESS_INDICATOR = lua_isnumber(L, Index);").

:- pragma foreign_proc("C", lua_isstring(Index::in, L::in),
	[promise_semipure, will_not_call_mercury],
	" SUCCESS_INDICATOR = lua_isstring(L, Index);").

:- pragma foreign_proc("C", lua_isinteger(Index::in, L::in),
	[promise_semipure, will_not_call_mercury],
"
	if(lua_isnumber(L, Index));
	 SUCCESS_INDICATOR = 
	 	!(lua_tonumber(L, Index) - lua_tointeger(L, Index));
").

:- pragma foreign_proc("C", lua_isthread(Index::in, L::in),
	[promise_semipure, will_not_call_mercury],
	"SUCCESS_INDICATOR = lua_isthread(L, Index);").

:- pragma foreign_proc("C", lua_isfunction(Index::in, L::in),
	[promise_semipure, will_not_call_mercury],
	"SUCCESS_INDICATOR = lua_isfunction(L, Index);").

:- pragma foreign_proc("C", lua_isnil(Index::in, L::in),
	[promise_semipure, will_not_call_mercury],
	"SUCCESS_INDICATOR = lua_isnil(L, Index);").

:- pragma foreign_proc("C", lua_isuserdata(Index::in, L::in),
	[promise_semipure, will_not_call_mercury],
	"SUCCESS_INDICATOR = lua_isuserdata(L, Index);").

:- pragma foreign_proc("C", lua_istable(Index::in, L::in),
	[promise_semipure, will_not_call_mercury],
	"SUCCESS_INDICATOR = lua_istable(L, Index);").

:- pragma foreign_proc("C", lua_islightuserdata(Index::in, L::in),
	[promise_semipure, will_not_call_mercury],
	"SUCCESS_INDICATOR = lua_islightuserdata(L, Index);").

:- pragma foreign_proc("C", lua_isboolean(Index::in, L::in),
	[promise_semipure, will_not_call_mercury],
	"SUCCESS_INDICATOR = lua_isboolean(L, Index);").

:- pragma foreign_proc("C", lua_iscfunction(Index::in, L::in),
	[promise_semipure, will_not_call_mercury],
	"SUCCESS_INDICATOR = lua_iscfunction(L, Index);").

%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C", lua_tonumber(Index::in, L::in, V::out),
	[promise_semipure, will_not_call_mercury],
"
	 V = lua_tonumber(L, Index);
").

:- pragma foreign_proc("C", lua_tostring(Index::in, L::in, V::out),
	[promise_semipure, will_not_call_mercury],
"
	 V = lua_tostring(L, Index);
").

:- pragma foreign_proc("C", lua_tointeger(Index::in, L::in, V::out),
	[promise_semipure, will_not_call_mercury],
"
	 V = lua_tointeger(L, Index);
").

	% Pull thread value.
	%
:- semipure pred lua_tothread(lua_state::in, index::in, lua_state::out) is det.

:- pragma foreign_proc("C", lua_tothread(Index::in, L::in, V::out),
	[promise_semipure, will_not_call_mercury],
"
	 V = lua_tothread(L, Index);
").

	% Pull function value.
	%
:- semipure pred lua_tofunction(lua_state::in, index::in, function::out) is det.

:- pragma foreign_proc("C", lua_tofunction(Index::in, L::in, V::out),
	[promise_semipure, will_not_call_mercury],
"
	 V = lua_tofunction(L, Index);
").

	
	% Pull userdata value.
	%
:- semipure pred lua_touserdata(lua_state::in, index::in, T::out) is det.

:- pragma foreign_proc("C", lua_touserdata(Index::in, L::in, V::out),
	[promise_semipure, will_not_call_mercury],
"
	 V = lua_touserdata(L, Index);
").

	% Pull table value.
	%
:- semipure pred lua_totable(lua_state::in, index::in, table::out) is det.

lua_totable(L, I, table(L, Ref)) :-
	semipure lua_toref(L, I, R).

	% Pull lightuserdata value.
	%
:- semipure pred lua_tolightuserdata(lua_state::in, c_pointer::out) is det.

:- pragma foreign_proc("C", lua_tolightuserdata(Index::in, L::in, V::out),
	[promise_semipure, will_not_call_mercury],
"
	 V = lua_tolightuserdata(L, Index);
").

	% Pull boolean value.
	%
:- semipure pred lua_toboolean(lua_state::in, boolua::out) is det.

:- pragma foreign_proc("C", lua_toboolean(Index::in, L::in, V::out),
	[promise_semipure, will_not_call_mercury],
"
	 if(lua_toboolean(L, Index))
	 	V = MR_YES;
	 else
	 	V = MR_NO;
").

	% Pull cfunction value.
	%
:- semipure pred lua_tocfunction(lua_state::in, c_function::out) is det.

:- pragma foreign_proc("C", lua_tocfunction(Index::in, L::in, V::out),
	[promise_semipure, will_not_call_mercury],
"
	 V = lua_tocfunction(L, Index);
").


	% Pull ref value.
	%
:- semipure pred lua_toref(lua_state::in, ref::out) is det.

:- pragma foreign_proc("C", lua_toref(Index::in, L::in, V::out),
	[promise_semipure, will_not_call_mercury],
"
	 V = luaMR_new_ref(L, Index);
").


%-----------------------------------------------------------------------------%

	% Push number value.
	%
:- impure pred lua_pushnumber(lua_state::in, float::in) is det.

:- pragma foreign_proc("C", lua_pushnumber(lua::in, V::in),
	[will_not_call_mercury],
"
	 lua_pushnumber(L, V);
").

	% Push string value.
	%
:- impure pred lua_pushstring(lua_state::in, string::in) is det.

:- pragma foreign_proc("C", lua_pushstring(lua::in, V::in),
	[will_not_call_mercury],
"
	 lua_pushstring(L, V);
").

	% Push integer value.
	%
:- impure pred lua_pushinteger(lua_state::in, int::in) is det.

:- pragma foreign_proc("C", lua_pushinteger(lua::in, V::in),
	[will_not_call_mercury],
"
	 lua_pushinteger(L, V);
").

	% Push thread value.
	%
:- impure pred lua_pushthread(lua_state::in, lua_state::in) is det.

:- pragma foreign_proc("C", lua_pushthread(lua::in, V::in),
	[will_not_call_mercury],
"
	 lua_pushthread(L, V);
").

	% Push function value.
	%
:- impure pred lua_pushfunction(lua_state::in, function::in) is det.

:- pragma foreign_proc("C", lua_pushfunction(lua::in, V::in),
	[will_not_call_mercury],
"
	 lua_pushfunction(L, V);
").

	% Push nil value.
	%
:- impure pred lua_pushnil(lua_state::in, nilua::in) is det.

:- pragma foreign_proc("C", lua_pushnil(lua::in, V::in),
	[will_not_call_mercury],
"
	 lua_pushnil(L, V);
").

	% Push userdata value.
	%
:- impure pred lua_pushuserdata(lua_state::in, T::in) is det.

:- pragma foreign_proc("C", lua_pushuserdata(lua::in, V::in),
	[will_not_call_mercury],
"
	 lua_pushuserdata(L, V);
").

	% Push table value.
	%
:- impure pred lua_pushtable(lua_state::in, table::in) is det.

:- pragma foreign_proc("C", lua_pushtable(lua::in, V::in),
	[will_not_call_mercury],
"
	 lua_pushtable(L, V);
").

	% Push lightuserdata value.
	%
:- impure pred lua_pushlightuserdata(lua_state::in, c_pointer::in) is det.

:- pragma foreign_proc("C", lua_pushlightuserdata(lua::in, V::in),
	[will_not_call_mercury],
"
	 lua_pushlightuserdata(L, V);
").

	% Push boolean value.
	%
:- impure pred lua_pushboolean(lua_state::in, boolua::in) is det.

:- pragma foreign_proc("C", lua_pushboolean(lua::in, V::in),
	[will_not_call_mercury],
"
	 if(V == MR_YES)
	 	lua_pushboolean(L, 1);
	 else
	 	lua_pushboolean(L, 0);
").

	% Push cfunction value.
	%
:- impure pred lua_pushcfunction(lua_state::in, c_function::in) is det.

:- pragma foreign_proc("C", lua_pushcfunction(lua::in, V::in),
	[will_not_call_mercury],
"
	 lua_pushcfunction(L, V);
").


	% Push ref value.
	%
:- impure pred lua_pushref(lua_state::in, ref::in) is det.

:- pragma foreign_proc("C", lua_pushref(lua::in, V::in),
	[will_not_call_mercury],
"
	 luaMR_lua_pushref(L, V);
").


%-----------------------------------------------------------------------------%

