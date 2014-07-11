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
% This file implements a manner to compile loadable Lua modules in the 
% Mercury programming language.
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
:- import_module univ.


	% Represents the state of Lua, this is passed to and from C as 
	% a lua_State *
	%
:- type lua_state.

	% Prepares a lua_State for interaction with Mercury
	%
:- pred init(lua_state::in, io::di, io::uo) is det.

	% Verify that Lua is prepared for interaction with Mercury
	%
:- pred lua_ready(lua_state::in) is semidet.

	% Passes a value to a lua_State's module loaders under the 
	% given string name
	%
:- pred export_module(lua_state::in, string::in, T::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

	% Represents a refrence to a variable instantiated in Lua.
	%
:- type var where equality is raw_equal.


	% In Lua, equality comparisons may induce side effects due to the
	% usage of metatables. Explicitly calling raw_equal ensures a pure
	% equality comparison.
	%
:- pred raw_equal(var::in, var::in) is semidet.

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

:- pred type(var::in, lua_type::out) is det.
:- func type(var) = lua_type.

%-----------------------------------------------------------------------------%


	% Through the Lua State, a mercury value can be passed as the value 
	% to a new Lua var.  int, float, bool, string and c_pointer are passed
	% by value, while other types are passed by refrence.
	%
	% The following predicates will pass Mercury values to lua, instantiate
	% them in Lua variables, and then return a refrence to the new Lua 
	% variable back to Mercury.
	%
	% The Lua state may be omitted to deconstruct a lua variable
	%
	% Example construction:  	L ^ int(3) = Var 
	% Example deconstruction: 	Var = int(N), N = 3
	% Alternate deconstruction: 	to_int(Var) = 3


:- pred nil(lua_state, var).
:- mode nil(in, out) is det.
:- mode nil(out, in) is semidet.
:- pred nil(var::in) is semidet.
:- func nil(lua_state) = var is det.
:- func nil = (var::in) is semidet.
	
:- pred int(lua_state, int, var).
:- mode int(in, in, out) is det.
:- mode int(out, out, in) is semidet.
:- func int(lua_state, int) = var is det.
:- pred int(int::out, var::in) is semidet.
:- func int(int::out) = (var::in) is semidet.
:- func to_int(var) = int is semidet.

:- pred float(lua_state, float, var).
:- mode float(in, in, out) is det.
:- mode float(out, out, in) is semidet.
:- func float(lua_state, float) = var is det.
:- pred float(float::out, var::in) is semidet.
:- func float(float::out) = (var::in) is semidet.
:- func to_float(var) = float is semidet.

:- pred bool(lua_state, bool, var).
:- mode bool(in, in, out) is det.
:- mode bool(out, out, in) is semidet.
:- func bool(lua_state, bool) = var is det.
:- pred bool(bool::out, var::in) is semidet.
:- func bool(bool::out) = (var::in) is semidet.
:- func to_bool(var) = bool is semidet.

:- pred string(lua_state, string, var).
:- mode string(in, in, out) is det.
:- mode string(out, out, in) is semidet.
:- func string(lua_state, string) = var is det.
:- pred string(string::out, var::in) is semidet.
:- func string(string::out) = (var::in) is semidet.
:- func to_string(var) = string is semidet.

:- pred c_pointer(lua_state, c_pointer, var).
:- mode c_pointer(in, in, out) is det.
:- mode c_pointer(out, out, in) is semidet.
:- func c_pointer(lua_state, c_pointer) = var is det.
:- pred c_pointer(c_pointer::out, var::in) is semidet.
:- func c_pointer(c_pointer::out) = (var::in) is semidet.
:- func to_pointer(var) = c_pointer is semidet.



	% A polymorphic alternative to the above predicates, T will be tested
	% against each of the above mentioned mercury types. Note that there is
	% no deconstructive version of these predicates.
	%
:- pred var(lua_state::in, T::in, var::out) is det.
:- func var(lua_state, T) = var is det.


%-----------------------------------------------------------------------------%

	% Create a Lua function by passing a mercury value
	%
:- pred function(lua_state::in, T::in, var::out) is det <= function(T).
:- func function(lua_state, T) = var is det <= function(T).

	% Typeclass for values that can be passed to Lua to construct
	% Lua functions.
	%
:- typeclass function(T) where [
	func call_function(T, A) = R <= (args(A), return(R))
].
	
	% Typeclass for values that can be passed from Lua as function
	% arguments.
	%
:- typeclass args(T) where [
	func args(lua_state) = T
].

	% Typeclass for values that can be passed back to Lua as a 
	% function return value.
	%
:- typeclass return(T) where [
	func return(lua_state, T) = return
].


	% Acceptable return values for a Lua function
	%
:- type return
	--->	nil
	;	return(var)
	;	return(list(var))
	;	error(string).
	











	% A typedef for a C function pointer that may be passed to Lua as 
	% a Lua function as defined in the Lua C API.
	%
:- type c_function.

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
:- pred load_string(lua_state::in, string::in, var::out) is semidet.
:- func load_string(lua_state, string) = var is semidet.

%TODO: Implement a version of load_string that accepts a 
% string_builder or stream

	% Pass a higher order call to Lua that may be called as a function.
	%
:- func export_pred(pred(lua_state, list(var))) = function.
:- mode export_pred(pred(in, out) is det) = (out) is det.
:- mode export_pred(pred(in, out) is semidet) = (out) is det.
% :- mode export_pred(pred(in, out) is multi) = (out) is det.
% :- mode export_pred(pred(in, out) is nondet) = (out) is det.

	% The same as above, but with 
:- func export_io_pred(pred(lua_state, list(var), io, io)) = function.
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
:- pred list_args(lua_state::in, list(var)::out) is det.
:- func list_args(lua_state) = list(var).


	% Extract values from tables (without calling metamethods), note that
	% the function will return nil if the key does not exist. 
	% If the variable is not a table, the call will always fail.
	%
:- pred get(var, T, var).
:- mode get(in, in, in) is semidet.
:- mode get(in, in, out) is semidet. 
:- mode get(in, out, in) is nondet.
:- mode get(in, out, out) is nondet.

:- func get(var, T) = var.
:- mode get(in, in) = in is semidet.
:- mode get(in, out) = out is semidet.
:- mode get(in, out) = in is nondet.
:- mode get(in, out) = out is nondet.

:- func var ^ T = var.
:- mode in ^ in = in is semidet.
:- mode in ^ in = out is semidet.
:- mode in ^ out = in is nondet.
:- mode in ^ out = out is nondet.




:- pred userdata(lua_state::in, T::in, var::out) is det.
:- func userdata(lua_state, T) = var is det.

:- some [T] pred userdata(T::out, var::in) is semidet.
:- some [T] func userdata(T::out) = (var::in) is semidet.


%-----------------------------------------------------------------------------%

:- implementation.

:- import_module set.

:- include_module userdata.
:- import_module userdata.



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



:- pragma foreign_type("C", c_function, "lua_CFunction").

:- pragma foreign_decl("C", "extern void luaAP_init(lua_State *);").

:- pragma foreign_code("C", "void luaAP_init(lua_State * L) {

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
}").

:- pragma foreign_proc("C", init(L::in, _I::di, _O::uo), 
	[promise_pure, will_not_call_mercury], "luaAP_init(L);").

:- pragma foreign_decl("C", "int luaAP_ready(lua_State *);").

:- pragma foreign_code("C", "
	/* check to see if Apollo has already been initialized. */
	int luaAP_ready(lua_State * L) {
		lua_checkstack(L, 1);
		lua_getfield(L, LUA_REGISTRYINDEX, AP_READY);
		int ready = lua_toboolean(L, 1);
		lua_remove(L, 1);
		return ready;
	}
").

:- pragma foreign_proc("C", lua_ready(L::in, _I::di, _O::uo), 
	[promise_pure, will_not_call_mercury], "
	SUCCESS_INDICATOR = luaAP_lua_

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

%-----------------------------------------------------------------------------%

:- pragma foreign_type("C", var, "luaAP_Var").

:- pragma foreign_proc("C", raw_equal(A::in, B::in),
	[promise_pure, will_not_call_mercury], "
	lua_State * L = luaAP_var_state(A);
	lua_checkstack(L, 2);
	luaAP_push_var(L, A);	
	luaAP_push_var(L, B);
	SUCCESS_INDICATOR = lua_rawequal(L, -2, -1);
	lua_pop(2);
").
	



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


:- pragma foreign_proc("C", type(V:in, T::out), 
	[promise_pure, will_not_call_mercury], "
	lua_State * L = luaAP_var_state(V);
	luaAP_push_var(V);
	T = lua_type(L, -1);
	lua_pop(L, 1);
").

type(V) = T :- type(V, T).

%-----------------------------------------------------------------------------%

	% This mutvar keeps a refrence to any Mercury variable passed to Lua
	% to ensure that Mercury does not garbage collect said variable before
	% Lua is finished with it.
	%
:- mutable(reserved, map(univ, int), set.init, ground, [untrailed]).

:- impure pred intern(univ::in) is det.

intern(U) :- semipure get_reserved(R),
	if search(R, U, I) then impure set_reserved(det_update(R, U, I + 1)) 
	else impure set_reserved(det_insert(R, U, 1)).

:- impure pred release(univ::in) is det.

release(U) :- semipure get_reserved(R),
	if search(R, U, I) then ( 
		if I > 1 then impure set_reserved(det_update(R, U, I - 1))
		else impure set_reserved(delete(R, U))
	) else error(
	"Attempted to release a Mercury value that was not interned.").

:- impure pred push_univ(lua_state::in, univ::in).


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

to_int(V) = I :- int(I, V).

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

to_float(V) = F :- float(F, V).

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

to_bool(V) = B :- bool(B, V).

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

to_string(V) = S :- string(S, V).

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

to_pointer(V) = P :- c_pointer(P, V).





%-----------------------------------------------------------------------------%

% function(lua_state::in, T::in, var::out)

:- type mr_function ---> some(T) (mr_function(T) => function(T)).

function(L, T, V) :- F = 'new mr_function'(T), intern(F), make_function(L, F, V).

:- pred make_function(lua_state::in, mr_function::in, var::out).

:- pragma foreign_proc("C", make_function(L::in, F::in, V::out), 
	[promise_pure, will_not_call_mercury], "
	

:- pred lua_call_function(lua_state::in, mr_function::in) is det.








