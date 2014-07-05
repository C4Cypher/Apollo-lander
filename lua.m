%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2014 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: lua.m.
% Main author: C4Cypher.
% Stability: low.
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module lua.

:- interface.

:- import_module io.
:- import_module int.
:- import_module float.
:- import_module bool.
:- import_module string.
:- import_module list.
:- import_module assoc_list.
:- import_module univ.

	% Represents the state of Lua, this is passed to and from C as 
	% a lua_State *
	%
:- type lua_state.

	% Prepares a lua_State for interaction with Mercury
	%
:- pred init_lua(lua_state::in, io::di, io::uo) is det.

	% Passes a value to a lua_State's module loaders under the 
	% given string name
	%
:- pred export_module(lua_state::in, string::in, T::in, io::di, io::uo) is det.

	% Represents a refrence to a variable instantiated in Lua.
	%
:- type lua_var where equality is raw_equal.


	% In Lua, equality comparisons may induce side effects due to the
	% usage of metatables. Explicitly calling raw_equal ensures a pure
	% equality comparison.
	%
:- pred raw_equal(lua_var::in, lua_var::in) is semidet.

	% In Lua, variables are not typed, values are.  Lua recognizes eight
	% types.
	%
	% 'nil' represents the abscence of value. 
	% 'number' is equivalent to float type.
	% 'boolean' is equivalent to the bool type.
	% 'string' is equivalent to the string type.
	%
	% 'table', a refrence to an associative array, used for lists and maps.
	%	Unlike an assoc_list, a Lua table may not associate more than
	%	one value with any given key, and thus, behaves more like the
	% 	Mercury map type.
	% 
	% 'function' is a refrence to a Lua function, which are impure, 
	%       may have a variable number of arguments, multiple return
	%	values and can be passed freely like any other variable.
	%
	% 'userdata' is lua's type for handling foreign data, unless otherwise
	%	noted, values stored as userdata are subject to collection by 		% 	Lua's Garbage Collector.
	%
	% 'lightuserdata' is seen in Lua as identical to userdata, but contains
	%	a C pointer which will not be collected by Lua's GC.
	%
	% 'thread' is a lua_state, usually a coroutine, note that the main
	% lua_state should not be treated like a coroutine.
	%
:- type lua_type 
	--->	none
	;	nil
	;	boolean
	;	lightuserdata
	;	number
	;	string
	;	table
	;	function
	;	userdata
	;	thread.

:- pred type(lua_var::in, lua_type::out) is det.
:- func type(lua_var) = lua_type.


	% Through the Lua State, a mercury value can be passed as the value 
	% to a new lua_var.  int, float, bool, string and c_pointer are passed
	% by value, while other types are passed by refrence.
	%
	% The following predicates will pass Mercury values to lua, instantiate
	% them in Lua variables, and then return a refrence to the new Lua 
	% variable back to Mercury.
	%
	% The Lua state is not required to deconstruct a lua variable
	%
	% Example usage:  L ^ int(3) = Var, Var = int(3)

:- pred nil(lua_state::in, lua_var::out) is det.
:- func nil(lua_state) = lua_var is det.

:- pred nil(lua_var::in) is semidet.
:- func nil = (lua_var::in) is semidet.
	
:- pred int(lua_state::in, int::in, lua_var::out) is det.
:- func int(lua_state, int) = lua_var is det.
	
:- pred int(int::out, lua_var::in) is semidet.
:- func int(int::out) = (lua_var::in) is semidet.

:- pred float(lua_state::in, float::in, lua_var::out) is det.
:- func float(lua_state, float) = lua_var is det.

:- pred float(float::out, lua_var::in) is semidet.
:- func float(float::out) = (lua_var::in) is semidet.

:- pred bool(lua_state::in, bool::in, lua_var::out) is det.
:- func bool(lua_state, bool) = lua_var is det.

:- pred bool(bool::out, lua_var::in) is semidet.
:- func bool(bool::out) = (lua_var::in) is semidet.

:- pred string(lua_state::in, string::in, lua_var::out) is det.
:- func string(lua_state, string) = lua_var is det.

:- pred string(string::out, lua_var::in) is semidet.
:- func string(string::out) = (lua_var::in) is semidet.

:- pred c_pointer(lua_state::in, c_pointer::in, lua_var::out) is det.
:- func c_pointer(lua_state, c_pointer) = lua_var is det.

:- pred c_pointer(c_pointer::out, lua_var::in) is semidet.
:- func c_pointer(c_pointer::out) = (lua_var::in) is semidet.


:- pred userdata(lua_state::in, T::in, lua_var::out) is det.
:- func userdata(lua_state, T) = lua_var is det.

:- some [T] pred userdata(T::out, lua_var::in) is semidet.
:- some [T] func userdata(T::out) = (lua_var::in) is semidet.

	% A polymorphic alternative to the above predicates, T will be tested
	% against each of the above mentioned mercury types. Note that there is
	% no deconstructive version of these predicates.
	%
:- pred var(lua_state::in, T::in, lua_var::out) is det.
:- func var(lua_state, T) = lua_var is det.


	% Type to be thrown if Lua throws an error while interacting with
	% Mercury.
	%
:- type lua_error ---> error(message::string, code::lua_error_code).

	% 'runtime' represents a standard runtime error.
	% 'memory' represents a memory allocation error.
	% 'error' represents an error called while trying to handle an
	%	error.
	% 
:- type lua_error_code
	--->	runtime
	;	memory
	;	error.

	% Accepts a string of Lua source code that is dynamically compiled
	% into a Lua Function.  This function may not refrence any upvalues
	% or global variables.
	%
:- pred load_string(lua_state::in, string::in, lua_var::out) is semidet.
:- func load_string(lua_state, string) = lua_var is semidet.

%TODO: Implement a version of load_string that accepts a 
% string_builder or stream

	% Pass a higher order call to Lua that may be called as a function.
	%
:- func export_pred(pred(lua_state, list(lua_var))) = function.
:- mode export_pred(pred(in, out) is det) = (out) is det.
:- mode export_pred(pred(in, out) is semidet) = (out) is det.
% :- mode export_pred(pred(in, out) is multi) = (out) is det.
% :- mode export_pred(pred(in, out) is nondet) = (out) is det.

	% The same as above, but with 
:- func export_io_pred(pred(lua_state, list(lua_var), io, io)) = function.
:- mode export_io_pred(pred(in, out, di, uo) is det) = (out) is det.
:- mode export_io_pred(pred(in, out, di, uo) is semidet) = (out) is det.
% :- mode export_io_pred(pred(in, out) is multi) = (out) is det.
% :- mode export_io_pred(pred(in, out) is nondet) = (out) is det.

	% For collecting arguments passed from Lua to Mercury in a 
	% function call.
	%
:- type args.

:- pred args(lua_state::in, args::out) is semidet.
:- func args(lua_state) = args is semidet.

:- pred arg(args::in, args::out, T::out) is semidet.
:- func arg(args::in, args::out) = (T::out) is semidet.

	% An alternative method
	%
:- pred list_args(lua_state::in, list(lua_var)::out) is det.
:- func list_args(lua_state) = list(lua_var).


	% Extract values from tables (without calling metamethods), note that
	% the function will return nil if the key does not exist. 
	% If the variable is not a table, the call will always fail.
	%
:- pred get(lua_var, T, lua_var).
:- mode get(in, in, in) is semidet.
:- mode get(in, in, out) is semidet. 
:- mode get(in, out, in) is nondet.
:- mode get(in, out, out) is nondet.

:- func get(lua_var, T) = lua_var.
:- mode get(in, in) = in is semidet.
:- mode get(in, out) = out is semidet.
:- mode get(in, out) = in is nondet.
:- mode get(in, out) = out is nondet.

:- func lua_var ^ T = lua_var.
:- mode in ^ in = in is semidet.
:- mode in ^ in = out is semidet.
:- mode in ^ out = in is nondet.
:- mode in ^ out = out is nondet.





%%%%%%%%%%%%%%%%%%
:- implementation.
%%%%%%%%%%%%%%%%%%

:- pragma foreign_decl("C", "
#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

#include ""lua_var.h""

#define AP_TYPE ""luaAPOLLO_LANDER_MERCURY_TYPE""
#define AP_MODULE ""luaAPOLLO_LANDER_MODULE""
#define AP_READY ""luaAPOLLO_LANDER_READY""
#define AP_LOCKED ""luaAPOLLO_LANDER_LOCKED""

").

:- pragma foreign_code("C", "

/* take the provided module name and attempt to load an apollo module
passes any additional arguments. */
int luaAP_loader(lua_State * L) {
	if (lua_isstring(L, 1)) {
		const char * module_name = lua_tostring(L, 1);
		lua_getfield(L, LUA_REGISTRYINDEX, AP_MODULE);
		lua_getfield(L, 2, module_name);
		return 1;
	}
	return 0;
}

";).

:- pragma foreign_type("C", lua_state, "lua_State *").

	% A typedef containing the signature for a C function pointer that may
	% be passed to Lua as a Lua function
	%
:- type c_function.

:- pragma foreign_type("C", c_function, "lua_CFunction").

:- pragma foreign_proc("C", init_lua(L::in, _I::di, _O::uo), 
	[promise_pure, will_not_call_mercury], "

	/* Add tables to the registry. */
	lua_newtable(L);
	lua_setfield(L, LUA_REGISTRYINDEX, AP_TYPE);
	
	lua_newtable(L);
	lua_setfield(L, LUA_REGISTRYINDEX, AP_MODULE);

	
	lua_pushboolean(L, 0);
	lua_setfield(L, LUA_REGISTRYINDEX, AP_LOCKED);

	/* Add loader to package.loaders */
	lua_getglobal(L, ""package"");
	lua_getfield(L, 1, ""loaders"");
	const lua_Integer length = (lua_Integer)lua_objlen(L, 1);
	lua_pushinteger(L, length + 1);
	lua_pushcfunction(L, luaAP_loader);
	lua_settable(L, 2);
	lua_pop(L, 2);
	
	/* Mark Apollo as ready */
	lua_pushboolean(L, 1);
	lua_setfield(L, LUA_REGISTRYINDEX, AP_READY);
").

:- pragma foreign_export("C", lua_init(in), "luaAP_init").

:- pragma foreign_proc("C", export_module(L::in, Name::in, M::in, _::di, _::uo),
	[promise_pure, will_not_call_mercury], "

	/* Push the module table onto the stack, then the module name and the
		module itself. Assign the module, by name to the module table.*/
	lua_getfield(L, LUA_REGISTRYINDEX, AP_MODULE);
	lua_pushstring(L, M);
	/* TODO Fix this:
	luaAP_push(L, M, luaAP_mercury_type(M)); */
	lua_setfield(L, -3);
	lua_pop(L, 1);
").


:- pragma foreign_type("C", lua_var, "luaAP_Var").


:- pragma foreign_proc("C", nil(L:in, V::out), 
	[promise_pure, will_not_call_mercury], "
	lua_pushnil(L);
	V = luaAP_new_var(L);
").

nil(L) = V :- nil(L, V).

:- pragma foreign_proc("C", nil(V::in), 
	[promise_pure, will_not_call_mercury], "
	lua_State * L = luaAP_var_state(V);
	luaAP_push_var(L, V);
	SUCCESS_INDICATOR = lua_isnil(L, -1);
	lua_pop(L, 1);
").

nil = V :- nil(V).

:- pragma foreign_proc("C", int(L:in, I::in, V::out), 
	[promise_pure, will_not_call_mercury], "
	lua_pushinteger(L, (lua_Integer)I);
	V = luaAP_new_var(L);
").

int(L, I) = V :- int(L, I, V).

:- pragma foreign_proc("C", int(I::out, V::in), 
	[promise_pure, will_not_call_mercury], "
	lua_State * L = luaAP_var_state(V);
	luaAP_push_var(L, V);
	if(lua_isnumber(L, -1) {
		double number = (double)lua_tonumber(L, -1);
		lua_pop(L, 1);
		I = (MR_Integer)number;
		SUCCESS_INDICATOR = !(I - number);
	} else {
		lua_pop(L, 1);
		SUCCESS_INDICATOR = 0;
	}
		
").

int(I) = V :- int(I, V).

:- pragma foreign_proc("C", float(L:in, F::in, V::out), 
	[promise_pure, will_not_call_mercury], 
	"lua_pushnumber(L, (lua_Number)F);
	V = luaAP_new_var(L);
").

float(L, F) = V :- float(L, F, V).

:- pragma foreign_proc("C", float(F::out, V::in), 
	[promise_pure, will_not_call_mercury], "
	lua_State * L = luaAP_var_state(V);
	luaAP_push_var(L, V);
	if(lua_isnumber(L, -1) {
		F = (MR_Float)lua_tonumber(L, -1);
		lua_pop(L, 1);
		SUCCESS_INDICATOR = 1;
	} else {
		lua_pop(L, 1);
		SUCCESS_INDICATOR = 0;
	}
		
").

float(F) = V :- float(F, V).

:- pragma foreign_proc("C", bool(L:in, B::in, V::out), 
	[promise_pure, will_not_call_mercury], "
	if (B == MR_YES)
		lua_pushboolean(L, 1);
	else
		lua_pushboolean(L, 0);
	V = luaAP_new_var(L);
").

bool(L, B) = V :- bool(L, B, V).

:- pragma foreign_proc("C", bool(B::out, V::in), 
	[promise_pure, will_not_call_mercury], "
	lua_State * L = luaAP_var_state(V);
	luaAP_push_var(L, V);
	if(lua_isboolean(L, -1) {
		int boolean = lua_toboolean(L, -1);
		lua_pop(L, 1);
		if(boolean) 
			B = MR_YES;
		else
			B = MR_NO;
		SUCCESS_INDICATOR = 1;
	} else {
		lua_pop(L, 1);
		SUCCESS_INDICATOR = 0;
	}
		
").

bool(B) = V :- bool(B, V).

:- pragma foreign_proc("C", string(L:in, S::in, V::out), 
	[promise_pure, will_not_call_mercury], 
	"lua_pushstring(L, S);
	V = luaAP_new_var(L);
").

string(L, S) = V :- string(L, S, V).

:- pragma foreign_proc("C", string(S::out, V::in), 
	[promise_pure, will_not_call_mercury], "
	lua_State * L = luaAP_var_state(V);
	luaAP_push_var(L, V);
	if(lua_isstring(L, -1) {
		S = (MR_String)lua_tostring(L, -1);
		lua_pop(L, 1);
		SUCCESS_INDICATOR = 1;
	} else {
		lua_pop(L, 1);
		SUCCESS_INDICATOR = 0;
	}
		
").

string(S) = V :- string(S, V).

:- pragma foreign_proc("C", c_pointer(L:in, P::in), 
	[promise_pure, will_not_call_mercury], 
	"lua_pushlightuserdata(L, P);
	V = luaAP_new_var(L);
").

c_pointer(L, P) = V :- c_pointer(L, P, V).


:- pragma foreign_proc("C", c_pointer(P::out, V::in), 
	[promise_pure, will_not_call_mercury], "
	lua_State * L = luaAP_var_state(V);
	luaAP_push_var(L, V);
	if(lua_islightuserdata(L, -1) {
		P = lua_tolightuserdata(L, -1);
		lua_pop(L, 1);
		SUCCESS_INDICATOR = 1;
	} else {
		lua_pop(L, 1);
		SUCCESS_INDICATOR = 0;
	}
		
").

c_pointer(P) = V :- c_pointer(P, V).







:- pragma foreign_enum("C", lua_type, [
	none - "LUA_TNONE",
	nil - "LUA_TNIL",
	boolean - "LUA_TBOOLEAN",
	lightuserdata - "LUA_TLIGHTUSERDATA",
	number - "LUA_TNUMBER",
	string - "LUA_TSTRING",
	table - "LUA_TTABLE",
	function - "LUA_TFUNCTION",
	userdata - "LUA_TUSERDATA",
	thread - "LUA_TTHREAD"
]).


