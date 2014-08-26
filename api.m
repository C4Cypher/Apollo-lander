%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2014 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: api.m.
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

:- module api.

:- interface.

%:- import_module stream.

% Note: These methods are unsafe without a clear understanding of the workings
% of the Lua C api, and even then, they're still pretty unsafe.






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
:- semipure pred lua_posindex(lua::in, index::out) is nondet.

	% Negative valid stack indexes
:- semipure pred lua_negindex(lua::in, index::out) is nondet.

	% All valid stack indexes (not counting pseudoindex).
:- semipure pred lua_stackindex(lua::in, index::out) is nondet.

	% The index of the registry
:- func registryindex = index.

	% The index of the global environment
:- func globalindex = index.

:- func index(int) = index.

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
:- impure pred lua_settop(lua::in, int::in) is det.

	% Allocate free space on the stack if needed, fail if it cannot
:- semipure pred lua_checkstack(lua::in, int::in) is semidet.

	% Directly push values from a different stack index
:- impure pred 	lua_pushvalue(lua::in, index::in) is det.

	% Push a value onto the Lua stack
:- impure pred lua_push(lua::in, value::in) is det.

	% Push a variable onto the stack
:- impure pred lua_pushvar(lua::in, var::in) is det.

	% Pop a number of values off the stack.
:- impure pred 	lua_pop(lua::in, int::in) is det.

% Note: Use of lua_remove and lua_insert is highly discouraged when used with 
% this library, given that said operations impurely re-arrange the Lua stack
% in a manner that ignores restrictions that Mercury needs to interact with
% it purely. Furthermore, the indexes provided to the following procedures
% MUST be valid indexes on the stack, not pseudo-indexes such as 
% registryindex or globalindex.

	% Remove a value from the stack at a given index, shifting the elments
	% above it down(dangerous)
	%
:- impure pred lua_remove(lua::in, index::in) is det.

	% Pop a value from the top of the stack and use it to replace the
	% value at the given stack index, without disturbing the rest of the
	% stack.
:- impure pred lua_replace(lua::in, index::in) is det.

	% Pop a value from the top of the stack, and insert it at the
	% given stack index, shifting elements up.
:- impure pred lua_insert(lua::in, index::in) is det.



%-----------------------------------------------------------------------------%
%
% Accessing and manipulating variables 
%

	
	% The Lua type of a value on the stack
	%
:- semipure func lua_type(lua, index) = lua_type.
	
	% The Lua type of a stack value as a string
	%
:- semipure func lua_typename(lua, index) = string.

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

	% Access the array portion of a Lua table without invoking metamethods
	%
:- impure pred lua_rawgeti(lua::in, index::in, int::in) is det.
:- impure pred lua_rawseti(lua::in, index::in, int::in) is det.

	% Access Lua tables, if Raw is yes, metamethod invocations are avoided,
	% but an error is thrown if Table is not actually a table.
	%
:- impure pred lua_gettable(lua::in, index::in) is det.
:- impure pred lua_settable(lua::in, index::in) is det.
	
	% Access a value from a table using a string key
	%
:- impure pred lua_getfield(lua::in, index::in, string::in) is det.
:- impure pred lua_setfield(lua::in, index::in, string::in) is det.

	% Access metatables, may cause undefined behavior if used on types
	% that do not have metatables.
	%
:- impure pred lua_getmetatable(lua::in, index::in) is semidet.
:- impure pred lua_setmetatable(lua::in, index::in) is det.

	% Create an empty table and push it onto the stack.
	%
:- impure pred lua_newtable(lua::in) is det.

	% Pop a key from the top of the stack and push the key-value pair
	% corresponding to the 'next' value associated with the table at
	% the given index.
	%
:- impure pred lua_next(lua::in, index::in) is det.

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


	% Load a function from a string.
:- impure func lua_loadstring(lua, string) = status is det.

	% lua_call(L, Args, [Results])
	% call a function
:- impure func lua_call(lua, int, int) = int.
:- impure func lua_call(lua, int) = int.


	% lua_pcall(L, Args, [Results], Error) = Returned.
	% call a function with an error handler.
	%
:- impure func lua_pcall(lua, int, int, index) = int.
:- impure func lua_pcall(lua, int, index) = int.

	% Call a mercury function from C
	%
:- impure func mr_call(lua) = int.

	% cpcall(CFunc, LUdataIn, L) = LUdataOut
	%
	% Protected C call in Lua, passing a pointer (or MR_Word)
	% as the only argument.  
	% 
:- impure func lua_cpcall(lua, c_function, c_pointer) = c_pointer.

% TODO: Should lua_error/2/3 be failure?

	% Throw an error from Mercury to Lua, passing the value on the stack
	% as the error value.
	%
:- impure pred lua_error(lua::in) is erroneous.

	% Throw an error from Mercury to Lua, passing the given value
	% as the error value.
	%
:- impure pred lua_error(lua::in, T::in) is erroneous.




%-----------------------------------------------------------------------------%
%
% Utilites for the concrete Lua state.
%

	% Create a fresh, new , initialized lua.
	%
:- func lua_new = lua.

:- func lua_newstate = lua_state.

	% Destroy a lua
	%
:- impure pred lua_close(lua::in) is det.	


	% Return the Lua state's current status.
	%
:- semipure func lua_status(lua) = status. 


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

:- semipure pred lua_isnumber(lua::in, index::in) is semidet.
:- semipure pred lua_isnil(lua::in, index::in) is semidet.
:- semipure pred lua_isuserdata(lua::in, index::in) is semidet.
:- semipure pred lua_ismruserdata(lua::in, index::in) is semidet.
:- semipure pred lua_isinteger(lua::in, index::in) is semidet.
:- semipure pred lua_islightuserdata(lua::in, index::in) is semidet.
:- semipure pred lua_isstring(lua::in, index::in) is semidet.
:- semipure pred lua_istable(lua::in, index::in) is semidet.
:- semipure pred lua_isboolean(lua::in, index::in) is semidet.
:- semipure pred lua_isthread(lua::in, index::in) is semidet.
:- semipure pred lua_isfunction(lua::in, index::in) is semidet.
:- semipure pred lua_iscfunction(lua::in, index::in) is semidet.

:- semipure func lua_tonumber(lua, index) = float.
:- semipure func lua_touserdata(lua, index) = univ.
:- semipure func lua_tocuserdata(lua, index) = c_pointer.
:- semipure func lua_tointeger(lua, index) = int.
:- semipure func lua_tostring(lua, index) = string.
:- semipure func lua_toboolean(lua, index) = bool.
:- semipure func lua_tothread(lua, index) = lua.
:- semipure func lua_tocfunction(lua, index) = c_function.
:- semipure func lua_toref(lua, index) = ref.

:- impure pred lua_pushnil(lua::in) is det.
:- impure pred lua_pushnumber(lua::in, float::in) is det.
:- impure pred lua_pushuserdata(lua::in, T::in) is det.
:- impure pred lua_pushuniv(lua::in, univ::in) is det.
:- impure pred lua_pushinteger(lua::in, int::in) is det.
:- impure pred lua_pushlightuserdata(lua::in, c_pointer::in) is det.
:- impure pred lua_pushstring(lua::in, string::in) is det.
:- impure pred lua_pushboolean(lua::in, bool::in) is det.
:- impure pred lua_pushthread(lua::in) is det.
:- impure func lua_pushthread(lua) = bool.
:- impure pred lua_pushfunc(lua, lua_func).
:- mode lua_pushfunc(in, dfi) is det.
:- mode lua_pushfunc(in, sfi) is det.
:- impure pred lua_pushcfunction(lua::in, c_function::in) is det.
:- impure pred lua_pushcclosure(lua::in, c_function::in, int::in) is det.
:- impure pred lua_pushref(lua::in, ref::in) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module exception.
:- import_module solutions.

:- pragma foreign_decl("C", "
#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>
").

%-----------------------------------------------------------------------------%
%
% Stack indexes
%

lua_posindex(L, I) :-
	semipure T = lua_gettop(L),
	(
		I = 1
	;
		semipure lua_posindex(L, I - 1),
		I =< T
	).
	
		
lua_negindex(L, -P) :- semipure lua_posindex(L, P).

lua_stackindex(L, I) :- 
	semipure lua_posindex(L, I) ; semipure lua_negindex(L, I). 

:- pragma foreign_proc("C", registryindex = (I::out),
	[promise_pure, will_not_call_mercury], "I = LUA_REGISTRYINDEX;").
	
:- pragma foreign_proc("C", globalindex = (I::out),
	[promise_pure, will_not_call_mercury], "I = LUA_GLOBALSINDEX;").
	
index(I) = I.

%-----------------------------------------------------------------------------%
%
% Stack Manipulation
%

:- pragma foreign_proc("C", lua_gettop(L::in) = (Index::out),
	[promise_semipure, will_not_call_mercury],
	"Index = lua_gettop(L); ").
	
:- pragma foreign_proc("C",  lua_settop(L::in, Index::in),
	[will_not_call_mercury],
	"lua_settop(L, Index);").

:- pragma foreign_proc("C",  lua_checkstack(L::in, Free::in),
	[will_not_call_mercury, promise_semipure], "lua_checkstack(L, Free);").

:- pragma foreign_proc("C",  lua_pushvalue(L::in, I::in),
	[will_not_call_mercury], "lua_pushvalue(L, I);").
	
lua_push(L, V) :-
	require_complete_switch [V]
	( V = nil(_) ->
		impure lua_pushnil(L)
	; V = number(T) ->
		impure lua_pushnumber(L, T)
	; V = integer(T) ->
		impure lua_pushinteger(L, T)
	; V = boolean(T) ->
		impure lua_pushboolean(L, T)
	; V = string(T) ->
		impure lua_pushstring(L, T)
	; V = lightuserdata(T) ->
		impure lua_pushlightuserdata(L, T)
	; V = thread(T) ->
		(T = L -> impure lua_pushthread(L)
		; error("Can only push the active thread onto the stack.")
		)
	; V = c_function(T) ->
		impure lua_pushcfunction(L, T)
	; V = var(_) ->
		sorry($module, $pred, 
		"Implement the pushing of variables.") 
	; V = userdata(T) ->
		impure lua_pushuniv(L, T)
	; V = lua_error(T) ->
		impure lua_error(L, T)
	; V = unbound ->
		impure lua_pushuserdata(L, V)
	; unexpected($module, $pred, "Attempted to push an unexpected value: "
		++ string.string(V))
	).
	
lua_pushvar(L, V) :-
	require_complete_switch [V]
	( V = local(I) -> impure lua_pushvalue(L, I)
	; V = index(Val, Table) ->
		impure lua_pushvar(L, Table)

:- pragma foreign_proc("C",  lua_pop(L::in, Num::in),
	[will_not_call_mercury], "lua_pop(L, Num);").

:- pragma foreign_proc("C",  lua_remove(L::in, Index::in), 
	[will_not_call_mercury], "lua_remove(L, Index);").

:- pragma foreign_proc("C",  lua_replace(L::in, Index::in), 
	[will_not_call_mercury], "lua_replace(L, Index);").

:- pragma foreign_proc("C",  lua_insert(L::in, Index::in), 
	[will_not_call_mercury], "lua_insert(L, Index);").

%-----------------------------------------------------------------------------%
%
% Accessing and manipulating variables 
%

:- pragma foreign_proc("C",  lua_type(L::in, Index::in) = (Type::out), 
	[promise_semipure, will_not_call_mercury],
	"Type = lua_type(L, Index);").
	
:- pragma foreign_proc("C",  lua_typename(L::in, Index::in) = (Name::out), 
	[promise_semipure, will_not_call_mercury],
	"Name = (char *)lua_typename(L, lua_type(L, Index));").


:- pragma foreign_proc("C", lua_rawget(L::in, I::in), 
	[will_not_call_mercury], "lua_rawget(L, I);").
	 
:- pragma foreign_proc("C", lua_rawset(L::in, I::in), 
	[will_not_call_mercury], "lua_rawset(L, I);"). 

:- pragma foreign_proc("C", lua_rawgeti(L::in, I::in, N::in), 
	[will_not_call_mercury], "lua_rawgeti(L, I, N);").
	 
:- pragma foreign_proc("C", lua_rawseti(L::in, I::in, N::in), 
	[will_not_call_mercury], "lua_rawseti(L, I, N);"). 

:- pragma foreign_proc("C", lua_gettable(L::in, I::in), 
	[will_not_call_mercury], "lua_gettable(L, I);"). 
	
:- pragma foreign_proc("C", lua_settable(L::in, I::in), 
	[will_not_call_mercury], "lua_settable(L, I);"). 
	
:- pragma foreign_proc("C", lua_getfield(L::in, I::in, K::in), 
	[will_not_call_mercury], "lua_getfield(L, I, K);"). 
	
:- pragma foreign_proc("C", lua_setfield(L::in, I::in, K::in), 
	[will_not_call_mercury], "lua_setfield(L, I, K);"). 
	
:- pragma foreign_proc("C", lua_getmetatable(L::in, I::in), 
	[will_not_call_mercury], 
	"SUCCESS_INDICATOR = lua_getmetatable(L, I);"). 

:- pragma foreign_proc("C", lua_setmetatable(L::in, I::in), 
	[will_not_call_mercury], "
	lua_setmetatable(L, I);
	if(luaMR_ismruserdata(L, I))
		luaMR_set_userdata_metatable(L, I);
"). 

:- pragma foreign_proc("C", lua_newtable(L::in), 
	[will_not_call_mercury], "lua_newtable(L);"). 

:- pragma foreign_proc("C", lua_next(L::in, I::in), 
	[will_not_call_mercury], "lua_next(L, I);"). 

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

:- pragma foreign_proc("C", lua_loadstring(L::in, S::in) = (Success::out),
	[will_not_call_mercury], "Success = luaL_loadstring(L, S);").
	
:- pragma foreign_proc("C", lua_call(L::in, Args::in, Ret::in) = (Returned::out),
	[will_not_call_mercury], "
	int Start = lua_gettop(L) - Args - 1;
	lua_call(L, Args, Ret);
	Returned = lua_gettop(L) - Start;
	").
	
lua_call(L, A) = R :-
	impure R = lua_call(L, A, multret).

	
:- pragma foreign_proc("C", lua_pcall(L::in, Args::in, Ret::in, Err::in) 
	= (Returned::out),
	[will_not_call_mercury], "
	int Start = lua_gettop(L) - Args - 1;
	lua_pcall(L, Args, Ret, Err);
	Returned = lua_gettop(L) - Start;
	").
	
lua_pcall(L, A, E) = R :-
	impure R = lua_pcall(L, A, multret, E).

:- pragma foreign_proc("C", lua_cpcall(L::in, Func::in, Ptr::in) = (R::out),
	[will_not_call_mercury], "
	R = lua_cpcall(L, Func, (void *)Ptr);
	").


:- func multret = int.

:- pragma foreign_proc("C", multret = (M::out),
	[promise_pure, will_not_call_mercury], "M = LUA_MULTRET;").


mr_call(L) = R :- 
	promise_equivalent_solutions [E] (try(mr_call(L), E)),
	require_complete_switch [E]
	( E = succeeded(R) 
	; E = failed,
		impure lua_pushboolean(L, no),
		R = 1
	; E = exception(Ex),
		impure lua_pushnil(L),
		impure lua_pushuserdata(L, Ex),
		R = 2
	).
		

:- pred mr_call(lua::in, int::out) is semidet.
	
mr_call(L,  R) :- 
	impure lua_getupvalue(L, 1),
	semipure lua_touserdata(L, -1) = U,
	U = univ(FU) ->
		require_complete_switch [FU]
		( FU = det_func(F) ; FU = semidet_func(F) ),
		impure R = impure_apply(F,L)	
	; 
		error( 
		"Called Mercury function without valid func upvalue.").

:- pragma promise_pure(mr_call/2).
		
:- pragma foreign_export("C", mr_call(in) = out, "luaMR_call").	
	
:- func mr_call_ptr = c_function.

:- pragma foreign_proc("C", mr_call_ptr = (F::out),
	[promise_pure, will_not_call_mercury], "F = (lua_CFunction)luaMR_call;").

:- pragma foreign_proc("C", lua_error(L::in),
	[will_not_call_mercury],"lua_error(L);").
	
lua_error(L, T) :-
	impure lua_pushuserdata(L, T),
	impure lua_error(L). 
	

%-----------------------------------------------------------------------------%
%
% Utilites for the concrete Lua state.
%

	
	
:- func return_nil = nil.

return_nil = nil.

:- pragma foreign_export("C", return_nil = out, "luaMR_nil").

:- pragma foreign_proc("C", lua_new = (L::out),
	[promise_pure, will_not_call_mercury], "
	L = luaL_newstate();
	luaL_openlibs(L);
	luaMR_init(L);
	").
	
lua_newstate = { lua_new, CP } :- impure current_choicepoint_id = CP.

:- pragma promise_pure(lua_newstate/0).

:- pragma foreign_proc("C", lua_close(L::in),
	[will_not_call_mercury], "lua_close(L);").
	
:- pragma foreign_proc("C", lua_status(L::in) = (S::out),
	[promise_semipure, will_not_call_mercury], "S = lua_status(L);").

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

:- pragma foreign_proc("C", lua_ismruserdata(L::in, Index::in),
	[promise_semipure, will_not_call_mercury], "
	if(lua_isuserdata(L, Index) && lua_getmetatable(L, Index)) { 
		lua_pushstring(L, LUA_MR_USERDATA);
		lua_rawget(L, Index);
		SUCCESS_INDICATOR = lua_toboolean(L, -1); 
		lua_pop(L, 1);
	} else {
		SUCCESS_INDICATOR = 0;
	}
").
		
:- pragma foreign_export("C", lua_ismruserdata(in, in), 
	"luaMR_ismruserdata").
	
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
	[promise_semipure, will_not_call_mercury], "
	size_t  len;
	const char * S0 = lua_tolstring(L, Index, &len);
	char * S = MR_GC_malloc(len);
	V = strncpy(S, S0, len);
").

:- pragma foreign_proc("C", lua_tointeger(L::in, Index::in) = (V::out),
	[promise_semipure, will_not_call_mercury],
	"V = lua_tointeger(L, Index);").

:- pragma foreign_proc("C", lua_tothread(L::in, Index::in) = (V::out),
	[promise_semipure, will_not_call_mercury],
	"V = lua_tothread(L, Index);").

	
lua_touserdata(L, Index) = U :-
	semipure lua_ismruserdata(L, Index) -> 
		semipure U = lua_tomruserdata(L, Index)
	;
		semipure C = lua_tocuserdata(L, Index), throw("fuck you"),
		U = univ(C).

:- semipure func lua_tomruserdata(lua, index) = univ.
 	
:- pragma foreign_proc("C", lua_tomruserdata(L::in, Index::in) = (V::out),
	[promise_semipure, will_not_call_mercury],
	"V = **(MR_Word **)lua_touserdata(L, Index);").
	
:- pragma foreign_proc("C", lua_tocuserdata(L::in, Index::in) = (V::out),
	[promise_semipure, will_not_call_mercury],
	"V = (size_t)lua_touserdata(L, Index);").

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
	"V = (luaMR_Ref)luaMR_newref(L, Index);").


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

:- pragma foreign_proc("C", lua_pushthread(L::in),
	[will_not_call_mercury],
	"lua_pushthread(L);").

:- pragma foreign_proc("C", lua_pushthread(L::in) = (Main::out),
	[will_not_call_mercury], "
	if(lua_pushthread(L))
		Main = MR_YES;
	else
		Main = MR_NO;
").


:- pragma foreign_proc("C", lua_pushnil(L::in),
	[will_not_call_mercury],
	"lua_pushnil(L);").

lua_pushuserdata(L, V) :- 
	impure lua_pushuniv(L, univ(V)).

:- pragma foreign_proc("C", lua_pushuniv(L::in, V::in),
	[will_not_call_mercury], " 
	MR_Word * mr_ptr = luaMR_new(V);
	MR_Word ** lua_ptr = lua_newuserdata(L, sizeof(MR_Word **));
	*lua_ptr = mr_ptr;
	luaMR_set_userdata_metatable(L, -1);
	").
	

:- pragma foreign_proc("C", lua_pushlightuserdata(L::in, V::in),
	[will_not_call_mercury],
	"lua_pushlightuserdata(L, (void *)V);").
	
:- pragma foreign_proc("C", lua_pushboolean(L::in, V::in),
	[will_not_call_mercury],
"
	 if(V == MR_YES)
	 	lua_pushboolean(L, 1);
	 else
	 	lua_pushboolean(L, 0);").

lua_pushfunc(L, V) :- 
	impure lua_pushuserdata(L, func_udata(V)),
	impure lua_pushcclosure(L, mr_call_ptr, 1).

:- pragma foreign_proc("C", lua_pushcfunction(L::in, V::in),
	[will_not_call_mercury],
	"lua_pushcfunction(L, V);").
	
:- pragma foreign_proc("C", lua_pushcclosure(L::in, V::in, Up::in),
	[will_not_call_mercury],
	"lua_pushcclosure(L, V, Up);").

:- pragma foreign_proc("C", lua_pushref(L::in, V::in),
	[will_not_call_mercury],
	"luaMR_pushref(L, V);").

%-----------------------------------------------------------------------------%

:- impure pred set_userdata_metatable(lua::in, index::in) is det.

:- pragma foreign_proc("C", set_userdata_metatable(L::in, I::in),
	[will_not_call_mercury], "luaMR_set_userdata_metatable(L, I);").

:- pragma foreign_decl("C", "
void luaMR_set_userdata_metatable(lua_State *, int);
").

:- pragma foreign_code("C", "

void luaMR_set_userdata_metatable(lua_State * L, int I) {
	lua_pushvalue(L, I);
	
	if(!lua_getmetatable(L, -1))
		lua_newtable(L);
	
	
	
	lua_pushstring(L, LUA_MR_USERDATA);
	lua_pushboolean(L, 1);
	lua_rawset(L, -3);
	
	lua_pushstring(L, ""__GC"");
	lua_pushcfunction(L, (lua_CFunction)luaMR_free);
	lua_rawset(L, -3);
	
	lua_pushstring(L, ""__tostring"");
	lua_pushcfunction(L, (lua_CFunction)luaMR_tostring);
	lua_rawset(L, -3);
	
	lua_setmetatable(L, -2);
		
	lua_pop(L, 1);
}
").


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%




