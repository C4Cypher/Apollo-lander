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

	% The index of the registry
:- func registryindex = index.

	% The index of the global environment
:- func globalindex = index.

	% Pass an int as an index.
:- func index(int) = index.
:- mode index(in) = out is det.
:- mode index(out) = in is det.

	% Pass a int as an absolute (positive) index
:- semipure func absolute(int, lua) = index.
:- mode absolute(in, in) = out is det.
:- mode absolute(out, in) = in is det.


:- semipure pred lua_posindex(index, lua).
:- mode lua_posindex(out, in) is nondet.
:- mode lua_posindex(out, in) is cc_nondet.

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
:- impure pred lua_settop(int::in, lua::in) is det.

	% Allocate free space on the stack if needed, fail if it cannot
:- semipure pred lua_checkstack(int::in, lua::in) is semidet.

	% Throw an error if checkstack fails to allocate
:- semipure pred det_checkstack(int::in, lua::in) is det.

	% Directly push values from a different stack index
:- impure pred 	lua_pushvalue(index::in, lua::in) is det.

	% Pop a number of values off the stack.
:- impure pred 	lua_pop(int::in, lua::in) is det.

% Note: Use of lua_remove and lua_insert is highly discouraged when used with 
% this library, given that said operations impurely re-arrange the Lua stack
% in a manner that ignores restrictions that Mercury needs to interact with
% it purely. Furthermore, the indexes provided to the following procedures
% MUST be valid indexes on the stack, not pseudo-indexes such as 
% registryindex or globalindex.

	% Remove a value from the stack at a given index, shifting the elments
	% above it down(dangerous)
	%
:- impure pred lua_remove(index::in, lua::in) is det.

	% Pop a value from the top of the stack and use it to replace the
	% value at the given stack index, without disturbing the rest of the
	% stack.
:- impure pred lua_replace(index::in, lua::in) is det.

	% Pop a value from the top of the stack, and insert it at the
	% given stack index, shifting elements up.
:- impure pred lua_insert(index::in, lua::in) is det.

%-----------------------------------------------------------------------------%
%
%  Values and vars 
%

	% Push a luaMR.value onto the Lua stack
:- impure pred push_value(value::in, lua::in) is det.

	% Push a luaMR.var onto the stack
:- impure pred push_var(var::in, lua::in) is det.

	% Retreive the value at a given index. Tables and Functions will be 
	% converted to refs.
:- semipure func to_value(index, lua) = value.

	% Retreive the value at the given index. Tables and functions will be
	% passed by local stack index. 
:- semipure func local_value(index, lua) = value.

	% A given var is valid.
:- semipure pred valid_var(var, lua).
:- mode valid_var(in, in) is semidet.

	% table_value(Table, Key, Value, L)
	% all non-nil values assigned to a table.
	% fails if Table is not a table.
	%
:- semipure pred table_value(var, value, value, lua).
:- mode table_value(in, out, out, in) is nondet.

	% Return the lua_type of a var
:- semipure pred var_type(var::in, lua_type::out, lua::in) is det.
:- semipure func var_type(var, lua) = lua_type.


	% Check to see if the values at two indexes are equal.
:- semipure pred lua_rawequal(index::in, index::in, lua::in) is semidet. 

	% Test equality on vars (no metamethods)
:- semipure pred var_equal(var::in, var::in, lua::in) is semidet.

	% Test equality on values (no metamethods)
:- semipure pred value_equal(value::in, value::in, lua::in) is semidet.

%-----------------------------------------------------------------------------%
%
% Accessing and manipulating variables 
%

	
	% The Lua type of a value on the stack
	%
:- semipure func lua_type(index, lua) = lua_type.
	
	% The Lua type of a stack value as a string
	%
:- semipure func lua_typename(index, lua) = string.

% Get calls will remove the key from the top of the table and replace it
% with the value.
% 
% Lua: v = t[k]
% get(N, L) :: [... t, ... k] -> [... t, ... v] 
%                   N     -1          N     -1
%
% Set calls will remove both the key and the value from the top of the stack.
%
% Lua: t[k] = v
% set(N, L) :: [... t, ... _, k, v] -> [... t, ... _] 
%                   N      X  Y  Z          N      X

	
	% Access Lua tables without invoking metamethods
	%
:- impure pred lua_rawget(index::in, lua::in) is det.
:- impure pred lua_rawset(index::in, lua::in) is det.

	% Access the array portion of a Lua table without invoking metamethods
	%
:- impure pred lua_rawgeti(index::in, int::in, lua::in) is det.
:- impure pred lua_rawseti(index::in, int::in, lua::in) is det.

	% Access Lua tables, if Raw is yes, metamethod invocations are avoided,
	% but an error is thrown if Table is not actually a table.
	%
:- impure pred lua_gettable(index::in, lua::in) is det.
:- impure pred lua_settable(index::in, lua::in) is det.
	
	% Access a value from a table using a string key
	%
:- impure pred lua_getfield(index::in, string::in, lua::in) is det.
:- impure pred lua_setfield(index::in, string::in, lua::in) is det.

	% Access metatables, may cause undefined behavior if used on types
	% that do not have metatables.
	%
:- impure pred lua_getmetatable(index::in, lua::in) is semidet.
:- impure pred lua_setmetatable(index::in, lua::in) is det.

	% Create an empty table and push it onto the stack.
	%
:- impure pred lua_newtable(lua::in) is det.

	% Pop a key from the top of the stack and push the key-value pair
	% corresponding to the 'next' value associated with the table at
	% the given index.
	%
:- impure pred lua_next(index::in, lua::in) is det.

%-----------------------------------------------------------------------------%
%
% The registry, and upvalues.
%

	% Access the registry 
	%
:- impure pred lua_getregistry(string::in, lua::in) is det.
:- impure pred lua_setregistry(string::in, lua::in) is det.

	% Access an upvalue
	%
:- impure pred lua_getupvalue(int::in, lua::in) is semidet.
:- impure pred lua_setupvalue(int::in, lua::in) is det.

%-----------------------------------------------------------------------------%
%
% Function constructors, deconstructors, and calls 
%


	% Load a function from a string.
:- impure func lua_loadstring(string, lua) = status is det.

	% lua_call(Args, Results, L)
	% lua_call(Args, L) = Results]
	% call a function
:- impure pred lua_call(int::in, int::in, lua::in) is det.
:- impure func lua_call(int, lua) = int.

	% lua_pcall(Args, Results, Error_handler, L) = Result. 
	% lua_pcall(Args, Error_handler, L) = Result.
	% lua_pcall(Args, L) = Returned.
	% call a function with an error handler. If
	% no error handler is 
:- impure func lua_pcall(int, int, index, lua) = lua_result.
:- impure func lua_pcall(int, index, lua) = lua_result.
:- impure func lua_pcall(int, lua) = lua_result.


	% Call a mercury function from C
	%
:- impure func mr_call(lua) = int.

:- type lua_result
	--->	returned(int)
	;	returned_error(lua_error).

	% cpcall(CFunc, LUdataIn, L) = LUdataOut
	%
	% Protected C call in Lua, passing a pointer (or MR_Word)
	% as the only argument.  
	% 
:- impure func lua_cpcall(c_function, c_pointer, lua) = c_pointer.

	% Throw an error from Mercury to Lua, passing the value on the stack
	% as the error value.
	%
:- impure pred lua_error(lua::in) is erroneous.

	% Throw an error from Mercury to Lua, passing the given value
	% as the error value.
	%
:- impure pred lua_error(T::in, lua::in) is erroneous.




%-----------------------------------------------------------------------------%
%
% Utilites for the concrete Lua state.
%

	% Create a fresh, new , initialized lua.
	%
:- func lua_new = lua.


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

:- semipure pred lua_isnumber(index::in, lua::in) is semidet.
:- semipure pred lua_isnil(index::in, lua::in) is semidet.
:- semipure pred lua_isuserdata(index::in, lua::in) is semidet.
:- semipure pred lua_ismruserdata(index::in, lua::in) is semidet.
:- semipure pred lua_isinteger(index::in, lua::in) is semidet.
:- semipure pred lua_islightuserdata(index::in, lua::in) is semidet.
:- semipure pred lua_isstring(index::in, lua::in) is semidet.
:- semipure pred lua_istable(index::in, lua::in) is semidet.
:- semipure pred lua_isboolean(index::in, lua::in) is semidet.
:- semipure pred lua_isthread(index::in, lua::in) is semidet.
:- semipure pred lua_isfunction(index::in, lua::in) is semidet.
:- semipure pred lua_iscfunction(index::in, lua::in) is semidet.

:- semipure func lua_tonumber(index, lua) = float.
:- semipure func lua_touserdata(index, lua) = univ.
:- semipure func lua_tocuserdata(index, lua) = c_pointer.
:- semipure func lua_tointeger(index, lua) = int.
:- semipure func lua_tostring(index, lua) = string.
:- semipure func lua_toboolean(index, lua) = bool.
:- semipure func lua_tothread(index, lua) = lua.
:- semipure func lua_tocfunction(index, lua) = c_function.
:- semipure func lua_toref(index, lua) = ref.

:- impure pred lua_pushnil(lua::in) is det.
:- impure pred lua_pushnumber(float::in, lua::in) is det.
:- impure pred lua_pushuserdata(T::in, lua::in) is det.
:- impure pred lua_pushuniv(univ::in, lua::in) is det.
:- impure pred lua_pushinteger(int::in, lua::in) is det.
:- impure pred lua_pushlightuserdata(c_pointer::in, lua::in) is det.
:- impure pred lua_pushstring(string::in, lua::in) is det.
:- impure pred lua_pushboolean(bool::in, lua::in) is det.
:- impure pred lua_pushthread(lua::in) is det.
:- impure func lua_pushthread(lua) = bool.
:- impure pred lua_pushfunc(lua_func, lua).
:- mode lua_pushfunc(dfi, in) is det.
:- mode lua_pushfunc(sfi, in) is det.
:- impure pred lua_pushcfunction(c_function::in, lua::in) is det.
:- impure pred lua_pushcclosure(c_function::in, int::in, lua::in) is det.
:- impure pred lua_pushref(ref::in, lua::in) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module exception.
:- import_module solutions.

:- pragma foreign_import_module("C", luaMR).

:- pragma foreign_decl("C", "
#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>
").

%-----------------------------------------------------------------------------%
%
% Stack indexes
%



:- pragma foreign_proc("C", registryindex = (I::out),
	[promise_pure, will_not_call_mercury], "I = LUA_REGISTRYINDEX;").

:- pragma inline(registryindex/0).

:- pragma foreign_proc("C", globalindex = (I::out),
	[promise_pure, will_not_call_mercury], "I = LUA_GLOBALSINDEX;").

:- pragma inline(globalindex/0).

index(I) = I.

:- pragma inline(index/1).

:- pragma foreign_proc("C", absolute(I::in, L::in) = (A::out), 
	[promise_semipure, will_not_call_mercury], "
	A = I > 0 ? I : lua_gettop(L) + 1 + I;").
	
:- pragma foreign_proc("C", absolute(I::out, L::in) = (A::in), 
	[promise_semipure, will_not_call_mercury], "
	I = A > 0 ? I : lua_gettop(L) + 1 + I;").
	
:- pragma inline(absolute/2).
	
:- pragma foreign_decl("C", "
	int luaMR_absolute(lua_State *, int);").
	
:- pragma foreign_code("C", "
	int luaMR_absolute(lua_State * L, int I) {
		return I > 0 ? I : lua_gettop(L) + 1 + I;
	}
").


%-----------------------------------------------------------------------------%
%
% Stack Manipulation
%

:- pragma foreign_proc("C", lua_gettop(L::in) = (Index::out),
	[promise_semipure, will_not_call_mercury],
	"Index = lua_gettop(L); ").
	
:- pragma inline(lua_gettop/1). 
	
:- pragma foreign_proc("C",  lua_settop(Index::in, L::in),
	[will_not_call_mercury],
	"lua_settop(L, Index);").
	
:- pragma inline(lua_settop/2).

:- pragma foreign_proc("C",  lua_checkstack(Free::in, L::in),
	[will_not_call_mercury, promise_semipure], 
	"SUCCESS_INDICATOR = lua_checkstack(L, Free);").
	
:- pragma inline(lua_checkstack/2).

det_checkstack(Free, L) :-
	semipure lua_checkstack(Free, L) ->
		true
	; 
		throw(lua_error(memory_error, "Checkstack failed")).

:- pragma foreign_proc("C",  lua_pushvalue(I::in, L::in),
	[will_not_call_mercury], "lua_pushvalue(L, I);").
	
:- pragma inline(lua_pushvalue/2).

:- pragma foreign_proc("C",  lua_pop(Num::in, L::in),
	[will_not_call_mercury], "lua_pop(L, Num);").
	
:- pragma inline(lua_pop/2).

:- pragma foreign_proc("C",  lua_remove(Index::in, L::in), 
	[will_not_call_mercury], "lua_remove(L, Index);").
	
:- pragma inline(lua_remove/2).

:- pragma foreign_proc("C",  lua_replace(Index::in, L::in), 
	[will_not_call_mercury], "lua_replace(L, Index);").
	
:- pragma inline(lua_replace/2).

:- pragma foreign_proc("C",  lua_insert(Index::in, L::in), 
	[will_not_call_mercury], "lua_insert(L, Index);").
	
:- pragma inline(lua_insert/2).

lua_posindex(I, L) :-
		semipure Top = lua_gettop(L),
		posindex2(Top, I).
		
		
:- pred posindex2(int::in, int::out) is nondet.

posindex2(Top, I) :-
	Top > 0,
	(
		I = Top
	;
		posindex2(Top - 1, I)
	).

%-----------------------------------------------------------------------------%
%
% Values and vars 
%

push_value(V, L) :-
	require_complete_switch [V]
	( V = nil(_),
		impure lua_pushnil(L)
	; V = number(F),
		impure lua_pushnumber(F, L)
	; V = integer(I),
		impure lua_pushinteger(I, L)
	; V = boolean(B),
		impure lua_pushboolean(B, L)
	; V = string(S),
		impure lua_pushstring(S, L)
	; V = lightuserdata(P),
		impure lua_pushlightuserdata(P, L)
	; V = thread(L2),
		(L2 = L -> impure lua_pushthread(L)
		; error("Can only push the active thread onto the stack.")
		)
	; V = c_function(F),
		impure lua_pushcfunction(F, L)
	; V = var(Var),
		impure push_var(Var, L) 
	; V = userdata(U),
		impure lua_pushuniv(U, L)
	; V = lua_error(E),
		impure lua_error(E, L)
	; V = unbound,
		impure lua_pushuserdata(V, L)
	).
	
push_var(V, L) :-
	require_complete_switch [V]
	( V = local(I), impure lua_pushvalue(I, L)
	; V = index(Val, Table),
		( Val = nil(_) -> impure lua_pushnil(L)
		; Val = unbound -> throw(lua_error(runtime_error, 
			"attempt to index var " ++ string(Table) ++
			" by an unbound value."))
		; impure push_var(Table, L), semipure lua_isnil(-1, L) -> 
			throw(lua_error(
			runtime_error, "attempt to index var "
			++ string.string(Table) ++
			" (a nil value)."))
		;
			impure push_value(Val, L),
			impure lua_rawget(-2, L),
			impure lua_remove(-2, L)
		)
	; V = meta(Table),
		impure push_var(Table, L), 
		( impure lua_getmetatable(-1, L) ->
			impure lua_remove(-2, L)
		; 
			impure lua_pop(1, L),
			impure lua_pushnil(L)
		)
	; V = ref(R), impure lua_pushref(R, L)
	; V = global(S), 
		impure lua_pushvalue(globalindex, L),
		impure lua_pushstring(S, L),
		impure lua_rawget(-2, L),
		impure lua_remove(-2, L)
	; V = invalid(S),
		throw(lua_error(runtime_error, $module ++ "." ++ $pred ++
		" attempted to push invalid var: " ++ S))
	).

to_value(I, L) = V :-
	semipure to_value(I, to_refvar, L) = V.
	
local_value(I, L) = V :-
	semipure to_value(I, to_localvar, L) = V.



:- semipure func to_value(index, (semipure func(index, lua) = var), lua) 
	= value.
	
to_value(I0, ToVar, L) = V :-	
	semipure I = absolute(I0, L),
	semipure Type = lua_type(I, L),
	require_complete_switch [Type]
	( Type = none,
		unexpected($module, $pred, "Value at given index had no type.")
	; Type = nil_type,
		V = nil(nil)
	; Type = number_type,
		semipure F = lua_tonumber(I, L),
		V = number(F)
	; Type = boolean_type,
		semipure B = lua_toboolean(I, L),
		V = boolean(B)
	; Type = string_type,
		semipure S = lua_tostring(I, L),
		V = string(S)
	; Type = lightuserdata_type,
		semipure C = lua_tocuserdata(I, L),
		V = lightuserdata(C)
	; Type = function_type,
		( semipure lua_iscfunction(I, L) ->
			semipure F = lua_tocfunction(I, L),
			V = c_function(F)
		; semipure Var = semipure_apply(ToVar, I, L),
			V = var(Var)
		)
	; Type = table_type,
		semipure T = semipure_apply(ToVar, I, L),
		V = var(T)
	; Type = thread_type,
		semipure T = lua_tothread(I, L),
		V = thread(T)
	; Type = userdata_type,
		semipure U = lua_touserdata(I, L),
		V = userdata(U)
	).
	
	
:- semipure func to_localvar(index, lua) = var.

to_localvar(I, _) = V :- 
	V = local(I),
	semipure semipure_true.
	
:- semipure func to_refvar(index, lua) = var.

to_refvar(I, L) = V :-
	semipure R = lua_toref(I, L),
	V = ref(R).
	

:- pragma foreign_proc("C",  lua_rawequal(Index1::in, Index2::in, L::in), 
	[promise_semipure, will_not_call_mercury],
	"SUCCESS_INDICATOR = lua_rawequal(L, Index1, Index2);").
	
:- pragma inline(lua_rawequal/3).


valid_var(V, L) :- 
	require_complete_switch [V]
	 ( V = local(Local),
	 	semipure Top = lua_gettop(L),
	 	require_complete_switch [Local]
		( Local < 0, Local >= -Top
		; Local > 0, Local =< Top
		)
	; V = index(_, Table),
		semipure var_type(Table, table_type, L)
	; V = meta(Var), 
	not 
		( semipure var_type(Var, string_type, L)
		; semipure var_type(Var, number_type, L)
		; semipure var_type(Var, lightuserdata_type, L)
		; semipure var_type(Var, nil_type, L)
		)
	; V = ref(_)
	; V = global(_)
	; V = invalid(_), fail
	).	
		

table_value(Table, Key, Value, L) :-
	semipure det_checkstack(6, L),
	semipure var_type(Table, table_type, L),
	impure lua_newtable(L), % Memo set table
	impure push_var(Table, L), 
	impure lua_pushboolean(yes, L),
	impure lua_rawset(-3, L), % Set the next value
	semipure Next = lua_toref(-1, L),
	impure lua_pop(1, L),
	semipure table_value2(Table, Key, Value, Next, L).
	
:- pragma promise_semipure(table_value/4).
	 	
:- semipure pred table_value2(var, value, value, ref, lua).
:- mode table_value2(in, out, out, in, in) is nondet.

table_value2(Table, Key, Value, Last, L) :-
	impure push_var(Table, L),	% Table being iterated
	impure lua_pushref(Last, L), 	% Last key 
	impure lua_next(-2, L), % Pop the last key and push the next pair
	% The stack should now look like [Table, Key, Value]
	semipure lua_isnil(-2, L) ->	% Is there another pair?
		impure lua_pop(3, L), 	% Clear the stack
		fail			% There are no more pairs
	; 
		(
			semipure Key = to_value(-2, L),
			semipure Value = to_value(-1, L)
		;
			semipure Next =  lua_toref(-2, L),
			semipure table_value2(Table, Key, Value, Next, L)
		),
		impure lua_pop(3, L). % Clear the stack
	
:- pragma promise_semipure(table_value2/5).
	

var_type(V, T, L) :-
	impure push_var(V, L),
	semipure T = lua_type(-1, L),
	impure lua_pop(1, L).

:- pragma promise_semipure(var_type/3).
	

var_type(V, L) = T :- semipure var_type(V, T, L).
	
var_equal(V1, V2, L) :-
	impure push_var(V1, L),
	impure push_var(V2, L),
	semipure lua_rawequal(-1, -2, L) ->
		impure lua_pop(2, L)
	;
		impure lua_pop(2, L),
		fail.	 
		
:- pragma promise_semipure(var_equal/3).

value_equal(V1, V2, L) :-
	impure push_value(V1, L),
	impure push_value(V2, L),
	semipure lua_rawequal(-1, -2, L) ->
		impure lua_pop(2, L)
	;
		impure lua_pop(2, L),
		fail.	  

:- pragma promise_semipure(value_equal/3).


%-----------------------------------------------------------------------------%
%
% Accessing and manipulating variables 
%

:- pragma foreign_proc("C",  lua_type(Index::in, L::in) = (Type::out), 
	[promise_semipure, will_not_call_mercury],
	"Type = lua_type(L, Index);").
	
:- pragma inline(lua_type/2).
	
:- pragma foreign_proc("C",  lua_typename(Index::in, L::in) = (Name::out), 
	[promise_semipure, will_not_call_mercury],
	"Name = (char *)lua_typename(L, lua_type(L, Index));").
	
:- pragma inline(lua_typename/2).


:- pragma foreign_proc("C", lua_rawget(I::in, L::in), 
	[will_not_call_mercury], "lua_rawget(L, I);").
	
:- pragma inline(lua_rawget/2).
	 
:- pragma foreign_proc("C", lua_rawset(I::in, L::in), 
	[will_not_call_mercury], "lua_rawset(L, I);"). 
	
:- pragma inline(lua_rawset/2).

:- pragma foreign_proc("C", lua_rawgeti(I::in, N::in, L::in), 
	[will_not_call_mercury], "lua_rawgeti(L, I, N);").
	
:- pragma inline(lua_rawgeti/3).
	 
:- pragma foreign_proc("C", lua_rawseti(I::in, N::in, L::in), 
	[will_not_call_mercury], "lua_rawseti(L, I, N);"). 
	
:- pragma inline(lua_rawseti/3).

:- pragma foreign_proc("C", lua_gettable(I::in, L::in), 
	[may_call_mercury], "lua_gettable(L, I);"). 
	
:- pragma inline(lua_gettable/2).
	
:- pragma foreign_proc("C", lua_settable(I::in, L::in), 
	[may_call_mercury], "lua_settable(L, I);"). 
	
:- pragma inline(lua_settable/2).
	
:- pragma foreign_proc("C", lua_getfield(I::in, K::in, L::in), 
	[may_call_mercury], "lua_getfield(L, I, K);").
	
:- pragma inline(lua_getfield/3).
	
:- pragma foreign_proc("C", lua_setfield(I::in, K::in, L::in), 
	[will_not_call_mercury], "lua_setfield(L, I, K);"). 

:- pragma inline(lua_setfield/3).
	
:- pragma foreign_proc("C", lua_getmetatable(I::in, L::in), 
	[may_call_mercury], 
	"SUCCESS_INDICATOR = lua_getmetatable(L, I);"). 
	
:- pragma inline(lua_getmetatable/2).

:- pragma foreign_proc("C", lua_setmetatable(I0::in, L::in), 
	[may_call_mercury], "
	int I = luaMR_absolute(L, I0);
	lua_setmetatable(L, I);
	if(luaMR_ismruserdata(I, L))
		luaMR_set_userdata_metatable(L, I);
"). 

:- pragma foreign_proc("C", lua_newtable(L::in), 
	[will_not_call_mercury], "lua_newtable(L);"). 
	
:- pragma inline(lua_newtable/1).

:- pragma foreign_proc("C", lua_next(I::in, L::in), 
	[may_call_mercury], "lua_next(L, I);"). 
	
:- pragma inline(lua_next/2).

%-----------------------------------------------------------------------------%
%
% The registry, and upvalues.
%
 
:- pragma foreign_proc("C", lua_getregistry(I::in, L::in), 
	[will_not_call_mercury], "luaMR_getregistry(L, I);").
	
:- pragma inline(lua_getregistry/2).

:- pragma foreign_proc("C", lua_setregistry(I::in, L::in), 
	[will_not_call_mercury], "luaMR_setregistry(L, I);"). 
	
:- pragma inline(lua_setregistry/2).
	
	
:- pragma foreign_proc("C", lua_getupvalue(I::in, L::in), 
	[will_not_call_mercury], "
	SUCCESS_INDICATOR = luaMR_getupvalue(L, I);
"). 

:- pragma inline(lua_getupvalue/2).

:- pragma foreign_proc("C", lua_setupvalue(I::in, L::in), 
	[will_not_call_mercury], "luaMR_setupvalue(L, I);"). 
	
:- pragma inline(lua_setupvalue/2).

%-----------------------------------------------------------------------------%
%
% Function constructors, deconstructors, and calls 
%

:- pragma foreign_proc("C", lua_loadstring(S::in, L::in) = (Success::out),
	[may_call_mercury], "Success = luaL_loadstring(L, S);").
	
:- pragma inline(lua_loadstring/2).
	
:- pragma foreign_proc("C", lua_call(Args::in, Ret::in, L::in),
	[may_call_mercury], "lua_call(L, Args, Ret);").
	
:- pragma inline(lua_call/3).
	
lua_call(A, L) = R :-
	semipure T1 = lua_gettop(L),
	S = T1 - A - 1,
	impure lua_call(A, multret, L),
	semipure T2 = lua_gettop(L),
	R = T2 - S.


lua_pcall(A, R, E, L) = Result :-
	semipure T1 = lua_gettop(L),
	S = T1 - A - 1,
	impure Error = lua_pcall2(A, R, E, L),
	( Error = no_error ->
		semipure T2 = lua_gettop(L),
		Result = returned(T2 - S)
	; 
		semipure Message = lua_tostring(-1, L),
		impure lua_pop(1, L),
		Result = returned_error(lua_error(Error, Message))
	).

:- impure func lua_pcall2(int, int, index, lua) = error_type.

:- pragma foreign_proc("C", lua_pcall2(Args::in, Ret::in, Err::in, L::in) 
		= (Result::out),
	[may_call_mercury], " Result = lua_pcall(L, Args, Ret, Err);").
	
%:- pragma inline(lua_pcall/4).
	
lua_pcall(A, E, L) = R :-
	impure R = lua_pcall(A, multret, E, L).
	
lua_pcall(A, L) = R :-
	impure R = lua_pcall(A, 0, L).
	
:- pragma foreign_proc("C", lua_cpcall(Func::in, Ptr::in, L::in) = (R::out),
	[may_call_mercury], "
	R = lua_cpcall(L, Func, (void *)Ptr);
	").


:- func multret = int.

:- pragma foreign_proc("C", multret = (M::out),
	[promise_pure, will_not_call_mercury], "M = LUA_MULTRET;").
	
:- pragma inline(multret/0).


mr_call(L) = R :- 
	promise_equivalent_solutions [E] (try(mr_callpred(L), E)),
	require_complete_switch [E]
	( E = succeeded(R) 
	; E = failed,
		impure lua_pushboolean(no, L),
		R = 1
	; E = exception(Ex),
		impure lua_pushnil(L),
		impure lua_pushuserdata(Ex, L),
		R = 2
	).
		

:- pred mr_callpred(lua::in, int::out) is semidet.
	
mr_callpred(L, R) :- 
	impure lua_getupvalue(1, L),
	semipure lua_touserdata(-1, L) = U,
	U = univ(FU) ->
		require_complete_switch [FU]
		( FU = det_func(F) ; FU = semidet_func(F) ),
		impure R = impure_apply(F, L)	
	; 
		error( 
		"Called Mercury function without valid func upvalue.").
		
:- pragma promise_pure(mr_callpred/2).
		
:- pragma foreign_export("C", mr_call(in) = out, "luaMR_call").	
	
:- func mr_call_ptr = c_function.

:- pragma foreign_proc("C", mr_call_ptr = (F::out),
	[promise_pure, will_not_call_mercury], "F = (lua_CFunction)luaMR_call;").

:- pragma foreign_proc("C", lua_error(L::in),
	[may_call_mercury],"lua_error(L);").
	
:- pragma inline(lua_error/1).
	
lua_error(T, L) :-
	impure lua_pushuserdata(T, L),
	impure lua_error(L). 
	

%-----------------------------------------------------------------------------%
%
% Utilites for the concrete Lua state.
%

	
	
:- func return_nil = nil.

return_nil = nil.

:- pragma inline(return_nil/0).

:- pragma foreign_export("C", return_nil = out, "luaMR_nil").

:- pragma foreign_proc("C", lua_new = (L::out),
	[promise_pure, will_not_call_mercury], "
	void * ptr = MR_malloc(sizeof(ptr));
	L = lua_newstate((lua_Alloc)luaMR_alloc, ptr);
	luaL_openlibs(L);
	luaMR_init(L);
	").
	
:- pragma foreign_decl("C", "
	void * luaMR_alloc(void *, void *, size_t, size_t);").
	
:- pragma foreign_code("C", "
	void * luaMR_alloc(void * ud, void * ptr, 
			size_t osize, size_t nsize) {
		(void)ud;
		if(nsize == 0) {
			if(osize == 0)
				return NULL;
			else
				MR_GC_free(ptr);
				return NULL;
		} else {
			if(osize == 0) {
				ptr = MR_GC_malloc(nsize);
				return ptr;
			} else {
				ptr = MR_GC_realloc(ptr,nsize);
				return ptr;
			}
		}
	}
").
	
	
:- pragma inline(lua_new/0).
	

:- pragma foreign_proc("C", lua_close(L::in),
	[may_call_mercury], "lua_close(L);").
	
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

:- pragma foreign_proc("C", lua_isnumber(Index::in, L::in),
	[promise_semipure, will_not_call_mercury],
	" SUCCESS_INDICATOR = lua_isnumber(L, Index);").
	
:- pragma inline(lua_isnumber/2).

:- pragma foreign_proc("C", lua_isstring(Index::in, L::in),
	[promise_semipure, will_not_call_mercury],
	" SUCCESS_INDICATOR = lua_isstring(L, Index);").
	
:- pragma inline(lua_isstring/2).

:- pragma foreign_proc("C", lua_isinteger(Index::in, L::in),
	[promise_semipure, will_not_call_mercury],
"
	if(lua_isnumber(L, Index));
	 SUCCESS_INDICATOR = 
	 	!(lua_tonumber(L, Index) - lua_tointeger(L, Index));").
	 	
:- pragma inline(lua_isinteger/2).

:- pragma foreign_proc("C", lua_isthread(Index::in, L::in),
	[promise_semipure, will_not_call_mercury],
	"SUCCESS_INDICATOR = lua_isthread(L, Index);").
	
:- pragma inline(lua_isthread/2).

:- pragma foreign_proc("C", lua_isnil(Index::in, L::in),
	[promise_semipure, will_not_call_mercury],
	"SUCCESS_INDICATOR = lua_isnil(L, Index);").
	
:- pragma inline(lua_isnil/2).

:- pragma foreign_proc("C", lua_isuserdata(Index::in, L::in),
	[promise_semipure, will_not_call_mercury],
	"SUCCESS_INDICATOR = lua_isuserdata(L, Index);").
	
:- pragma inline(lua_isuserdata/2).

:- pragma foreign_proc("C", lua_ismruserdata(Index::in, L::in),
	[promise_semipure, will_not_call_mercury], "
	int Top = lua_gettop(L);
	lua_pushvalue(L, Index); /* 1 */	
	if(lua_isuserdata(L, -1) && lua_getmetatable(L, -1)) { /* 2 */ 
		lua_pushstring(L, LUA_MR_USERDATA);
		lua_rawget(L, -2);
		SUCCESS_INDICATOR = lua_toboolean(L, -1); 
		lua_settop(L, Top);
	} else {
		SUCCESS_INDICATOR = 0;
		lua_settop(L, Top);
	}
").
		
:- pragma foreign_export("C", lua_ismruserdata(in, in), 
	"luaMR_ismruserdata").
	
:- pragma foreign_proc("C", lua_istable(Index::in, L::in),
	[promise_semipure, will_not_call_mercury],
	"SUCCESS_INDICATOR = lua_istable(L, Index);").
	
:- pragma inline(lua_istable/2).

:- pragma foreign_proc("C", lua_islightuserdata(Index::in, L::in),
	[promise_semipure, will_not_call_mercury],
	"SUCCESS_INDICATOR = lua_islightuserdata(L, Index);").
	
:- pragma inline(lua_islightuserdata/2).
	
:- pragma foreign_proc("C", lua_isboolean(Index::in, L::in),
	[promise_semipure, will_not_call_mercury],
	"SUCCESS_INDICATOR = lua_isboolean(L, Index);").
	
:- pragma inline(lua_isboolean/2).
	
:- pragma foreign_proc("C", lua_isfunction(Index::in, L::in),
	[promise_semipure, will_not_call_mercury],
	"SUCCESS_INDICATOR = lua_isfunction(L, Index);").

:- pragma inline(lua_isfunction/2).

:- pragma foreign_proc("C", lua_iscfunction(Index::in, L::in),
	[promise_semipure, will_not_call_mercury],
	"SUCCESS_INDICATOR = lua_iscfunction(L, Index);").
	
:- pragma inline(lua_iscfunction/2).


%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C", lua_tonumber(Index::in, L::in) = (V::out),
	[promise_semipure, will_not_call_mercury],
	"V = lua_tonumber(L, Index);").
	
:- pragma inline(lua_tonumber/2).

:- pragma foreign_proc("C", lua_tostring(Index::in, L::in) = (V::out),
	[promise_semipure, will_not_call_mercury], "
	V = MR_copy_string(lua_tostring(L, Index));
").

:- pragma foreign_proc("C", lua_tointeger(Index::in, L::in) = (V::out),
	[promise_semipure, will_not_call_mercury],
	"V = lua_tointeger(L, Index);").
	
:- pragma inline(lua_tointeger/2).

:- pragma foreign_proc("C", lua_tothread(Index::in, L::in) = (V::out),
	[promise_semipure, will_not_call_mercury],
	"V = lua_tothread(L, Index);").

:- pragma inline(lua_tothread/2).
	
lua_touserdata(Index, L) = U :-
	semipure lua_ismruserdata(Index, L) -> 
		semipure U = lua_tomruserdata(Index, L)
	;
		semipure C = lua_tocuserdata(Index, L),
		U = univ(C).

:- semipure func lua_tomruserdata(index, lua) = univ.
 	
:- pragma foreign_proc("C", lua_tomruserdata(Index::in, L::in) = (V::out),
	[promise_semipure, will_not_call_mercury],
	"V = **(MR_Word **)lua_touserdata(L, Index);").
	
:- pragma inline(lua_touserdata/2).
	
:- pragma foreign_proc("C", lua_tocuserdata(Index::in, L::in) = (V::out),
	[promise_semipure, will_not_call_mercury],
	"V = (size_t)lua_touserdata(L, Index);").
	
:- pragma inline(lua_tocuserdata/2).

:- pragma foreign_proc("C", lua_toboolean(Index::in, L::in) = (V::out),
	[promise_semipure, will_not_call_mercury],
	"V = lua_toboolean(L, Index) ? MR_YES : MR_NO;").
	 	
:- pragma inline(lua_toboolean/2).
	 	
:- pragma foreign_proc("C", lua_tocfunction(Index::in, L::in) = (V::out),
	[promise_semipure, will_not_call_mercury],
	"V = lua_tocfunction(L, Index);").
	
:- pragma inline(lua_tocfunction/2).

:- pragma foreign_proc("C", lua_toref(Index::in, L::in) = (V::out),
	[promise_semipure, will_not_call_mercury],
	"V = (luaMR_Ref)luaMR_newref(L, Index);").
	
:- pragma inline(lua_toref/2).


%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C", lua_pushnumber(V::in, L::in),
	[will_not_call_mercury],
	"lua_pushnumber(L, V);").
	
:- pragma inline(lua_pushnumber/2).

:- pragma foreign_proc("C", lua_pushstring(V::in, L::in),
	[will_not_call_mercury],
	"lua_pushstring(L, V);").
	
:- pragma inline(lua_pushstring/2).

:- pragma foreign_proc("C", lua_pushinteger(V::in, L::in),
	[will_not_call_mercury],
	"lua_pushinteger(L, V);").
	
:- pragma inline(lua_pushinteger/2).

:- pragma foreign_proc("C", lua_pushthread(L::in),
	[will_not_call_mercury],
	"lua_pushthread(L);").
	
:- pragma inline(lua_pushthread/1).

:- pragma foreign_proc("C", lua_pushthread(L::in) = (Main::out),
	[will_not_call_mercury], "
	Main = lua_pushthread(L) ? MR_YES : MR_NO;").

:- pragma inline(lua_pushthread/1).

:- pragma foreign_proc("C", lua_pushnil(L::in),
	[will_not_call_mercury],
	"lua_pushnil(L);").
	
:- pragma inline(lua_pushnil/1).

lua_pushuserdata(V, L) :- 
	impure lua_pushuniv(univ(V), L).
	
:- pragma inline(lua_pushuserdata/2).

:- pragma foreign_proc("C", lua_pushuniv(V::in, L::in),
	[will_not_call_mercury], " 
	MR_Word * mr_ptr = luaMR_new(V);
	MR_Word ** lua_ptr = lua_newuserdata(L, sizeof(MR_Word **));
	*lua_ptr = mr_ptr;
	luaMR_set_userdata_metatable(L, -1);
	").
	

:- pragma foreign_proc("C", lua_pushlightuserdata(V::in, L::in),
	[will_not_call_mercury],
	"lua_pushlightuserdata(L, (void *)V);").
	
:- pragma inline(lua_pushlightuserdata/2).
	
:- pragma foreign_proc("C", lua_pushboolean(V::in, L::in),
	[will_not_call_mercury],
	"lua_pushboolean(L, V == MR_YES ? 1 : 0);").
	
:- pragma inline(lua_pushboolean/2).

lua_pushfunc(V, L) :- 
	impure lua_pushuserdata(func_udata(V), L),
	impure lua_pushcclosure(mr_call_ptr, 1, L).
	

:- pragma foreign_proc("C", lua_pushcfunction(V::in, L::in),
	[will_not_call_mercury],
	"lua_pushcfunction(L, V);").
	
:- pragma inline(lua_pushcfunction/2).
	
:- pragma foreign_proc("C", lua_pushcclosure(V::in, Up::in, L::in),
	[will_not_call_mercury],
	"lua_pushcclosure(L, V, Up);").
	
:- pragma inline(lua_pushcclosure/3).

:- pragma foreign_proc("C", lua_pushref(V::in, L::in),
	[will_not_call_mercury],
	"luaMR_pushref(L, V);").
	
:- pragma inline(lua_pushref/2).

%-----------------------------------------------------------------------------%

:- impure pred set_userdata_metatable(index::in, lua::in) is det.

:- pragma foreign_proc("C", set_userdata_metatable(I::in, L::in),
	[will_not_call_mercury], "luaMR_set_userdata_metatable(L, I);").

:- pragma inline(set_userdata_metatable/2).

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




