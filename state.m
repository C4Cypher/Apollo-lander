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

%-----------------------------------------------------------------------------%
%
% init and ready
%

	% Set up the Lua state so that it has all of the assigned values
	% Mercury needs to interact with it.
	%
:- pred init(lua_state::in, io::di, io::uo) is det.
:- impure pred lua_init(lua_state::in) is det.

	% Check to see if lua_init has been called on a Lua state.
	%
:- semipure pred ready(lua_state::in) is semidet.
:- pred ready(lua_state::in, bool::out, io::di, io::uo) is det.


:- interface.

% Visualizing the Lua stack
%
% The stack will be illustrated using list syntax, the numbers underneath
% represent the indexes used to refer to those values in the stack using 
% Lua api calls.
% 
% Example: [A, B, C, ... X, Y, Z]
%           1  2  3     -3 -2 -1
%
% Here a is at the bottom of the stack at index 1, and z is at the top of
% the stack at index -1.  0 is never a valid index
%
% I'll be using haskell style function arrows to illustrate stack operations
%
% Example:
% push(Z, L) :: [... X, Y] -> [... X, Y, Z]
%                   -2 -1         -3 -2 -1


%-----------------------------------------------------------------------------%
%
% Stack indexes
%

	% Positive valid stack indexes
:- semipure pred lua_posindex(lua::in, int::out) is nondet.

	% Negative valid stack indexes
:- semipure pred lua_negindex(lua::in, int::out) is nondet.

	% All valid stack indexes (not counting pseudoindex).
:- semipure pred lua_stackindex(lua::in, int::out) is nondet.

	% The 
:- func lua_registryindex = int.

:- implementation.


lua_posindex(L, I) :-
		semipure lua_gettop(L, I)
	;
		lua_posindex(L, I0),
		I = I0 - 1,
		I > 0.
		
lua_negindex(L, I) :- lua_posindex(L, -I).

lua_stackindex(L, I) :- lua_posindex(L, I) ; lua_negindex(L, I). 
:- interface.

%-----------------------------------------------------------------------------%
%
% Stack Manipulation
%




	% The index at the top of the stack.
:- semipure func lua_gettop(lua) = int.

	% settop(N, L) :: [... X, Y, Z] -> [... X, Y] 
	%                         N                N
	% settop(N + 2, L) :: [... X] -> [... X, nil, nil]
	%                          N          N  N+1  N+2
	%
:- impure pred 	lua_settop(lua::in, int::in) is det.

	% Allocate free space on the stack if needed, fail if it cannot
:- semipure pred lua_checkstack(lua::in, int::in) is semidet.

	% Directly push values from a different stack index
:- impure pred 	lua_pushvalue(lua::in, int::in) is det.

	% Push a value onto the Lua stack
:- impure pred lua_push(lua::in, value::in) is det.

	% Pop 
:- impure pred 	lua_pop(lua::in, int::in) is det.

% Note: Use of lua_remove and lua_insert is highly discouraged when used with 
% this library, given that said operations impurely re-arrange the Lua stack
% in a manner that ignores restrictions that Mercury needs to interact with
% it purely.

:- implementation.

:- pragma foreign_proc("C", lua_gettop(L::in, Index::out),
	[promise_semipure, will_not_call_mercury],
	"Index = lua_gettop(L); ").
	
:- pragma foreign_proc("C",  set_top(L::in, Index::in),
	[will_not_call_mercury],
	"lua_settop(L, Index);").

:- pragma foreign_proc("C",  lua_checkstack(L::in, Free::in),
	[will_not_call_mercury, promise_semipure], "lua_checkstack(L, Free);").

- pragma foreign_proc("C",  lua_pushvalue(L::in, I::in),
	[will_not_call_mercury], "lua_pushvalue(L, I);").

:- pragma foreign_proc("C",  pop(L::in, Num::in),
	[will_not_call_mercury], "lua_pop(L, Num);").
:- interface.


%-----------------------------------------------------------------------------%
%
% Accessing and manipulating variables 
%

	
	% The Lua type of a value on the stack
	%
:- semipure func lua_gettype(lua, int) = lua_type.

% Get calls will remove the key from the top of the table and replace it
% with the value.
% 
% Lua: v = t[k]
% get(L, N) :: [... t, ... k] -> [... t, ... v] 
%                   N     -1          N     -1
%
% Set calls will remove both the key and the value from the top of the stack.
%
% Lua: t[k] = v
% set(L, N) :: [... t, ... _, k, v] -> [... t, ... _] 
%                   N      X  Y  Z          N      X

	
	% Access Lua tables without invoking metamethods
	%
:- impure pred lua_rawget(lua::in, index::in) is det.
:- impure pred lua_rawset(lua::in, index::in) is det.

	% Access Lua tables, if Raw is yes, metamethod invocations are avoided,
	% but an error is thrown if Table is not actually a table.
	%
:- impure pred lua_gettable(lua::in, index::in) is det.
:- impure pred lua_settable(lua::in, index::in) is det.
	
	% Access metatables, may cause undefined behavior if used on types
	% that do not have metatables.
	%
:- impure pred lua_getmetatable(lua::in, index::in) is det.
:- impure pred lua_setmetatable(lua::in, index::in) is det.

%-----------------------------------------------------------------------------%
%
% The registry, and upvalues.
%

	% Access the registry 
	%
:- impure pred lua_getregistry(lua::in, string::in) is det.
:- impure pred lua_setregistry(lua::in, string::in) is det.

	% Access an upvalue
	%
:- impure pred lua_getupvalue(lua::in, int::in) is semidet.
:- impure pred lua_setupvalue(lua::in, int::in) is det.

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


*/

%-----------------------------------------------------------------------------%
%
% Utilites for the concrete Lua state.
%

	% Create a fresh, new , initialized lua_state.
	%
:- func lua_newstate = lua_state.

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
%
% Value passing 
%

:- semipure pred lua_isnumber(L::in, Index::in) is semidet.
:- semipure pred lua_isnil(L::in, Index::in) is semidet.
:- semipure pred lua_isuserdata(L::in, Index::in) is semidet.
:- semipure pred lua_isinteger(L::in, Index::in) is semidet.
:- semipure pred lua_islightuserdata(c_pointer::in, L::in) is semidet.
:- semipure pred lua_isstring(L::in, Index::in) is semidet.
:- semipure pred lua_isboolean(L::in, Index::in) is semidet.
:- semipure pred lua_isthread(L::in, Index::in) is semidet.
:- semipure pred lua_isfunction(L::in, Index::in) is semidet.
:- semipure pred lua_iscfunction(L::in, Index::in) is semidet.

:- semipure func lua_tonumber(L, index) = float.
:- semipure func lua_touserdata(L, index) = univ.
:- semipure func lua_tointeger(L, index) = int..
:- semipure func lua_tolightuserdata(L, index) = c_pointer.
:- semipure func lua_tostring(L, index) = string.
:- semipure func lua_toboolean(L, index) = bool.
:- semipure func lua_tothread(L, index) = lua_state.
:- semipure func lua_tocfunction(L, index) = c_function.
:- semipure func lua_toref(L, index) = ref.

:- impure pred lua_pushnil(L::in) is det.
:- impure pred lua_pushnumber(L::in, float::in) is det.
:- impure pred lua_pushuserdata(L::in, univ::in) is det.
:- impure pred lua_pushinteger(L::in, int::in) is det.
:- impure pred lua_pushlightuserdata(L::in) is det.
:- impure pred lua_pushstring(L::in, string::in) is det.
:- impure pred lua_pushboolean(L::in, bool::in) is det.
:- impure pred lua_pushfunction(L::in, (func(lua_state) = int)::in) is det.
:- impure pred lua_pushcfunction(L::in, c_function::in) is det.
:- impure pred lua_pushcclosure(L::in, c_function::in, int::in) is det.
:- impure pred lua_pushref(L::in, ref::in) is det.

%-----------------------------------------------------------------------------%


:- implementation.

:- import_module require.

%-----------------------------------------------------------------------------%
%
% init and ready
%

:- pragma foreign_proc("C", init(L::in, _I::di, _O::uo), 
	[promise_pure, will_not_call_mercury], "luaMR_init(L);").


:- pragma foreign_proc("C", init(L::in), 
	[promise_pure, will_not_call_mercury], "luaMR_init(L);").


:- pragma foreign_proc("C", ready(L::in), 
	[promise_pure, will_not_call_mercury], "
	SUCCESS_INDICATOR = luaMR_ready(L);
").


:- pragma foreign_proc("C", ready(L::in, Answer::out, _I::di, _O::uo), 
	[promise_pure, will_not_call_mercury], "
	if(luaMR_ready(L))
		Answer = MR_YES;
	else
		Answer = MR_NO;
").

:- pragma foreign_decl("C", "extern void luaMR_init(lua_State *);").

:- pragma foreign_decl("C", "int luaMR_ready(lua_State *);").


:- pragma foreign_code("C", "
void luaMR_init(lua_State * L) {
	
#ifdef BEFORE_502

	/* Set the Main thread in the registry */
	lua_pushthread(L, L);
	luaMR_setregistry(L, LUA_RIDX_MAINTHREAD);
	
	lua_pushvalue(L, LUA_GLOBALSINDEX);
	luaMR_setregistry(L, LUA_RIDX_GLOBALS);
	
	/* Add tables to the registry. */
	
	lua_newtable(L);
	luaMR_setregistry(L, MR_LUA_MODULE);

	/* TODO: Define and export luaMR_userdata_metatable. */
	luaMR_userdata_metatable(L);
	luaMR_setregistry(L, MR_LUA_UDATA);
	
	

	/* Add loader to package.loaders */
	lua_getglobal(L, ""package"");
	lua_getfield(L, 1, ""loaders"");
	const int length = luaMR_len(L, 1);
	lua_pushinteger(L, length + 1);
	lua_pushcfunction(L, luaMR_loader);
	lua_settable(L, 2);
	lua_pop(L, 2);
	
	/* Mark Lua as ready */
	lua_pushboolean(L, 1);
	luaMR_setregistry(L, MR_LUA_READY);
} 

").



:- pragma foreign_code("C", "
	/* Check to see if Lua has already been initialized. */
	int luaMR_ready(lua_State * L) {
		lua_checkstack(L, 1);
		luaMR_getregistry(L, MR_LUA_READY);
		int ready = lua_toboolean(L, 1);
		lua_remove(L, 1);
		return ready;
	}
").


%-----------------------------------------------------------------------------%
%
% Accessing and manipulating variables 
%

:- pragma foreign_proc("C",  get_type(L::in, Index::in) = (Type::out)), 
	[promise_semipure, will_not_call_mercury],
	"Type = lua_type(L, Index);").

:- pragma foreign_proc("C", lua_rawget(L::in, I::in), 
	[will_not_call_mercury], "lua_rawget(L, I);").
	 
:- pragma foreign_proc("C", lua_rawset(L::in, I::in), 
	[will_not_call_mercury], "lua_rawset(L, I);"). 

:- pragma foreign_proc("C", lua_gettable(L::in, I::in), 
	[will_not_call_mercury], "lua_gettable(L, I);"). 
:- pragma foreign_proc("C", lua_settable(L::in, I::in), 
	[will_not_call_mercury], "lua_settable(L, I);"). 
	
:- pragma foreign_proc("C", lua_getmetatable(L::in, I::in), 
	[will_not_call_mercury], "lua_getmetatable(L, I);"). 

:- pragma foreign_proc("C", lua_setmetatable(L::in, I::in), 
	[will_not_call_mercury], "lua_setmetatable(L, I);"). 

%-----------------------------------------------------------------------------%
%
% The registry, and upvalues.
%
 


:- pragma foreign_proc("C", lua_getregistry(L::in, I::in), 
	[will_not_call_mercury], "luaMR_getregistry(L, I);"). 

:- pragma foreign_proc("C", lua_setregistry(L::in, I::in), 
	[will_not_call_mercury], "luaMR_setregistry(L, I);"). 
	
	
:- pragma foreign_proc("C", lua_getupvalue(L::in, I::in), 
	[will_not_call_mercury], "
	SUCCESS_INDICATOR = luaMR_getupvalue(L, I);
"). 

:- pragma foreign_proc("C", lua_setupvalue(L::in, I::in), 
	[will_not_call_mercury], "luaMR_setupvalue(L, I);"). 

%-----------------------------------------------------------------------------%
%
% Function constructors, deconstructors, and calls 
%

% TODO

%-----------------------------------------------------------------------------%
%
% Utilites for the concrete Lua state.
%

	
	
:- func return_nil = nil.

return_nil = nil.

:- pragma foreign_export("C", return_nil = out, "luaMR_nil").


:- pragma foreign_proc("C", lua_newstate = (L::out), 
	[promise_pure, will_not_call_mercury], 	"L = luaL_newstate();").
	
:- pragma foreign_proc("C", lua_close(L::in), 
	[promise_pure, will_not_call_mercury], "lua_close(L);").

:- pragma foreign_proc("C", lua_status(L::in, Status::out), 
	[promise_semipure, will_not_call_mercury], "Status = lua_status(L);").

:- pragma foreign_enum("C", status/0, [
	ready - "0",
	yield - "LUA_YIELD",
	runtime_error - "LUA_ERRRUN",
	syntax_error - "LUA_ERRSYNTAX",
	memory_error - "LUA_ERRMEM",
	unhandled_error - "LUA_ERRERR"
] ).


%-----------------------------------------------------------------------------%
%
% Value Passing
%

:- pragma foreign_proc("C", lua_isnumber(L::in, Index::in),
	[promise_semipure, will_not_call_mercury],
	" SUCCESS_INDICATOR = lua_isnumber(L, Index);").

:- pragma foreign_proc("C", lua_isstring(L::in, Index::in),
	[promise_semipure, will_not_call_mercury],
	" SUCCESS_INDICATOR = lua_isstring(L, Index);").

:- pragma foreign_proc("C", lua_isinteger(L::in, Index::in),
	[promise_semipure, will_not_call_mercury],
"
	if(lua_isnumber(L, Index));
	 SUCCESS_INDICATOR = 
	 	!(lua_tonumber(L, Index) - lua_tointeger(L, Index));").

:- pragma foreign_proc("C", lua_isthread(L::in, Index::in),
	[promise_semipure, will_not_call_mercury],
	"SUCCESS_INDICATOR = lua_isthread(L, Index);").

:- pragma foreign_proc("C", lua_isnil(L::in, Index::in),
	[promise_semipure, will_not_call_mercury],
	"SUCCESS_INDICATOR = lua_isnil(L, Index);").

:- pragma foreign_proc("C", lua_isuserdata(L::in, Index::in),
	[promise_semipure, will_not_call_mercury],
	"SUCCESS_INDICATOR = lua_isuserdata(L, Index);").

:- pragma foreign_proc("C", lua_istable(L::in, Index::in),
	[promise_semipure, will_not_call_mercury],
	"SUCCESS_INDICATOR = lua_istable(L, Index);").

:- pragma foreign_proc("C", lua_islightuserdata(L::in, Index::in),
	[promise_semipure, will_not_call_mercury],
	"SUCCESS_INDICATOR = lua_islightuserdata(L, Index);").

:- pragma foreign_proc("C", lua_isboolean(L::in, Index::in),
	[promise_semipure, will_not_call_mercury],
	"SUCCESS_INDICATOR = lua_isboolean(L, Index);").
	
:- pragma foreign_proc("C", lua_isfunction(L::in, Index::in),
	[promise_semipure, will_not_call_mercury],
	"SUCCESS_INDICATOR = lua_isfunction(L, Index);").

:- pragma foreign_proc("C", lua_iscfunction(L::in, Index::in),
	[promise_semipure, will_not_call_mercury],
	"SUCCESS_INDICATOR = lua_iscfunction(L, Index);").


%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C", lua_tonumber(L::in, Index::in) = (V::out),
	[promise_semipure, will_not_call_mercury],
	"V = lua_tonumber(L, Index);").

:- pragma foreign_proc("C", lua_tostring(L::in, Index::in) = (V::out),
	[promise_semipure, will_not_call_mercury],
	"V = lua_tostring(L, Index);").

:- pragma foreign_proc("C", lua_tointeger(L::in, Index::in) = (V::out),
	[promise_semipure, will_not_call_mercury],
	"V = lua_tointeger(L, Index);").

:- pragma foreign_proc("C", lua_tothread(L::in, Index::in) = (V::out),
	[promise_semipure, will_not_call_mercury],
	"V = lua_tothread(L, Index);").

:- pragma foreign_proc("C", lua_tofunction(L::in, Index::in) = (V::out),
	[promise_semipure, will_not_call_mercury],
	"V = lua_tofunction(L, Index);").
	
:- pragma foreign_proc("C", lua_touserdata(L::in, Index::in) = (V::out),
	[promise_semipure, will_not_call_mercury],
	"V = lua_touserdata(L, Index);").

:- pragma foreign_proc("C", lua_tolightuserdata(L::in, Index::in) = (V::out),
	[promise_semipure, will_not_call_mercury],
	"V = lua_tolightuserdata(L, Index);").

:- pragma foreign_proc("C", lua_toboolean(L::in, Index::in) = (V::out),
	[promise_semipure, will_not_call_mercury],
	"if(lua_toboolean(L, Index))
	 	V = MR_YES;
	 else
	 	V = MR_NO;").
	 	
:- pragma foreign_proc("C", lua_tocfunction(L::in, Index::in) = (V::out),
	[promise_semipure, will_not_call_mercury],
	"V = lua_tocfunction(L, Index);").

:- pragma foreign_proc("C", lua_toref(L::in, Index::in) = (V::out),
	[promise_semipure, will_not_call_mercury],
	"V = luaMR_new_ref(L, Index);").


%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C", lua_pushnumber(L::in, V::in),
	[will_not_call_mercury],
	"lua_pushnumber(L, V);").

:- pragma foreign_proc("C", lua_pushstring(L::in, V::in),
	[will_not_call_mercury],
	"lua_pushstring(L, V);").

:- pragma foreign_proc("C", lua_pushinteger(L::in, V::in),
	[will_not_call_mercury],
	"lua_pushinteger(L, V);").

:- pragma foreign_proc("C", lua_pushthread(L::in, V::in),
	[will_not_call_mercury],
	"lua_pushthread(L, V);").

:- pragma foreign_proc("C", lua_pushnil(L::in, V::in),
	[will_not_call_mercury],
	"lua_pushnil(L, V);").

%TODO: Implement lua_pushuserdata

:- pragma foreign_proc("C", lua_pushuserdata(L::in, V::in),
	[will_not_call_mercury],
	"lua_pushstring(
	L, ""lua_pushuserdata/2 really needs to be implemented."");
	lua_error(L);").

:- pragma foreign_proc("C", lua_pushlightuserdata(L::in, V::in),
	[will_not_call_mercury],
	"lua_pushlightuserdata(L, V);").

:- pragma foreign_proc("C", lua_pushboolean(L::in, V::in),
	[will_not_call_mercury],
"
	 if(V == MR_YES)
	 	lua_pushboolean(L, 1);
	 else
	 	lua_pushboolean(L, 0);").

:- pragma foreign_proc("C", lua_pushfunction(L::in, V::in),
	[will_not_call_mercury],
	"lua_pushstring(
	L, ""lua_pushfunction/2 really needs to be implemented."");
	lua_error(L);").
	
:- pragma foreign_proc("C", lua_pushcfunction(L::in, V::in),
	[will_not_call_mercury],
	"lua_pushcfunction(L, V);").
	
:- pragma foreign_proc("C", lua_pushcclosure(L::in, V::in, Up::in),
	[will_not_call_mercury],
	"lua_pushcclosure(L, V, Up);").


:- pragma foreign_proc("C", lua_pushref(L::in, V::in),
	[will_not_call_mercury],
	"luaMR_lua_pushref(L, V);").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%




