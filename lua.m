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
%:- pred export_module(lua_state::in, string::in, T::in, io::di, io::uo) is det.

	% Throw an error in Lua. WARNING! This predicate sends an error to the
	% Lua runtime and then performs a longjump OUT of the invoked function!
	% I have no idea what effect longjumping out of Mercury code will have,
	% nor do I know yet what will happen if an attempt is made to invoke
	% Mercury after said longjump!
	%
	%  * UNSAFE *
	%
:- pred lua_error(lua_state::in, string::in) is erroneous.

%-----------------------------------------------------------------------------%

	% Represents a refrence to a variable instantiated in Lua.
	%
:- type var.


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
	%	noted, values stored as userdata are subject to collection by 	
	% 	Lua's Garbage Collector.
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
	%
	% The det_to_Value functions are identical to the to_Value functions,
	% Only they will will abort instead of failing. 

	% nil

:- pred nil(var, lua_state).
:- mode nil(out, in) is det.
:- mode nil(in, out) is semidet.

:- func nil(lua_state) = var.
:- mode nil(in) = out is det.
:- mode nil(out) = in is semidet.

:- pred nil(var::in) is semidet.
:- func nil = (var::in) is semidet.
	
	% int

:- pred int(int, var, lua_state).
:- mode int(in, out, in) is det.
:- mode int(out, in, out) is semidet.

:- func int(int, lua_state) = var.
:- mode int(in, in) = out is det.
:- mode int(out, out) = in is semidet.

:- pred to_int(var::in, int::out) is semidet.
:- func to_int(var) = int is semidet.

:- pred det_to_int(var::in, int::out) is det.
:- func det_to_int(var) = int is det.

	% float

:- pred float(float, var, lua_state).
:- mode float(in, out, in) is det.
:- mode float(out, in, out) is semidet.

:- func float(float, lua_state) = var.
:- mode float(in, in) = out is det.
:- mode float(out, out) = in is semidet.

:- pred to_float(var::in, float::out) is semidet.
:- func to_float(var) = float is semidet.

:- pred det_to_float(var::in, float::out) is det.
:- func det_to_float(var) = float is det.

	% bool

:- pred bool(bool, var, lua_state).
:- mode bool(in, out, in) is det.
:- mode bool(out, in, out) is semidet.

:- func bool(bool, lua_state) = var.
:- mode bool(in, in) = out is det.
:- mode bool(out, out) = in is semidet.

:- pred to_bool(var::in, bool::out) is semidet.
:- func to_bool(var) = bool is semidet.

:- pred det_to_bool(var::in, bool::out) is det.
:- func det_to_bool(var) = bool is det.

	% string

:- pred string(string, var, lua_state).
:- mode string(in, out, in) is det.
:- mode string(out, in, out) is semidet.

:- func string(string, lua_state) = var.
:- mode string(in, in) = out is det.
:- mode string(out, out) = in is semidet.

:- pred to_string(var::in, string::out) is semidet.
:- func to_string(var) = string is semidet.

:- pred det_to_string(var::in, string::out) is det.
:- func det_to_string(var) = string is det.

	% c_pointer

:- pred c_pointer(c_pointer, var, lua_state).
:- mode c_pointer(in, out, in) is det.
:- mode c_pointer(out, in, out) is semidet.

:- func c_pointer(c_pointer, lua_state) = var.
:- mode c_pointer(in, in) = out is det.
:- mode c_pointer(out, out) = in is semidet.

:- pred to_pointer(var::in, c_pointer::out) is semidet.
:- func to_pointer(var) = c_pointer is semidet.

:- pred det_to_pointer(var::in, c_pointer::out) is det.
:- func det_to_pointer(var) = c_pointer is det.

	% univ

:- pred univ(univ, var, lua_state).
:- mode univ(in, out, in) is det.
:- mode univ(out, in, out) is semidet.

:- func univ(univ, lua_state) = var.
:- mode univ(in, in) = out is det.
:- mode univ(out, out) = in is semidet.

:- pred to_univ(var::in, univ::out) is semidet.
:- func to_univ(var) = univ is semidet.

:- pred det_to_univ(var::in, univ::out) is det.
:- func det_to_univ(var) = univ is det.

	% A polymorphic alternative to the above predicates, T will be tested
	% against each of the above mentioned mercury types. 
	%
:- pred var(T, var, lua_state).
:- mode var(in, out, in) is det.
:- mode var(out, in, out) is semidet.

:- func var(T, lua_state) = var.
:- mode var(in, in) = out is det.
:- mode var(out, out) = in is semidet.

:- pred from_var(var::in, T::out) is semidet.
:- func from_var(var) = T is semidet.

:- pred det_from_var(var::in, T::out) is det.
:- func det_from_var(var) = T is det.


%-----------------------------------------------------------------------------%
	
	% Lua functions are either compiled chunks of Lua source code, or
	% are C function pointers as defined by the lua_CFunction typedef
	% they are varadic, accepting variable numbers of arguments and
	% return values.
	% 
	% Lua handles functions as first-class variables, they can be assigned
	% to variables and passed as function arguments or return values in
	% the same manner as any other value in Lua.
	% 
	% Lua functions are inherently impure.  Not only can they cause
	% side effects
	
	
	
	% Create a Lua function by passing a mercury value, note that Mercury
	% should not attempt to directly deconstruct
	%
:- pred function(lua_state::in, T::in, var::out) is det <= function(T).
:- func function(lua_state, T) = var is det <= function(T).

	% Typeclass for values that can be passed to Lua to construct
	% Lua functions.
	%
:- typeclass function(T) where [
	(some [A, R] pred call_function(T, A, R, io, io) 
		=> (args(A), return(R))),
	(mode call_function(in, in, out, di, uo) is det)
].
	
	% Typeclass for values that can be passed from Lua as function
	% arguments.
	%
:- typeclass args(T) where [
	func args(lua_state) = T 
].

:- instance args(lua_state).

	% Typeclass for values that can be passed back to Lua as a 
	% function return value.
	%
:- typeclass return(T) where [
	func return(lua_state, T) = return
].

:- instance return(return).

	% Acceptable return values for a Lua function
	%
:- type return
	--->	nil	
	;	return_var(var)
	;	return_list(list(var))
	;	return_error(string).
	

	% A typedef for a C function pointer that may be passed to Lua as 
	% a Lua function as defined in the Lua C API.
	%
:- type c_function.

% TODO: Either move c_function to the implementation, or add predicates for it.

% TODO: Deconstruct functions into string/c_function/univ?

%-----------------------------------------------------------------------------%

% TODO: coroutines?, iterators?, multi/nondet functions?

%-----------------------------------------------------------------------------%

	% Lua handles all foreign values with the userdata value type.
	% Under normal circumstances, Lua can't actually do anything with
	% userdata. Attempts to use indexing, arithmetic or other operators
	% will result in a lua error.  However, as with tables and functions,
	% it is possible to assign metatables to userdata, allowing one to
	% extend the valid syntax with which Lua can interact with a given
	% userdata.
	
	% TODO: implement the userdata(T) type.

/*  TODO Change metamethod names to avoid conflicts with Mercury
:- type metamethod
	--->	add	%	+
	;	sub	%	-
	;	mul	%	*
	;	div	%	/
	; 	mod	%	%
	;	pow	%	^
	;	unm	%	-
	;	concat	%	..
	;	len	%	#
	;	eq	%	==
	;	lt	%	<
	;	le	%	<=
	;	index	%	u[k]
	;	newindex%	u[k] = v
	;	call	%	u(...)
	;	gc.
*/



%-----------------------------------------------------------------------------%

:- implementation.

:- import_module map.
:- import_module type_desc.

:- pragma foreign_decl("C", 
"
#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

#include <gc.h>

#define AP_USERDATA ""luaAPOLLO_LANDER_MERCURY_USERDATA""
#define AP_MODULE ""luaAPOLLO_LANDER_MODULE""
#define AP_READY ""luaAPOLLO_LANDER_READY""
#define AP_LOCKED ""luaAPOLLO_LANDER_LOCKED""

#define AP_MERCURY_UNIV ""__mercury_univ""

#define AP_FUNCTION_UPVALUE 1

").

:- pragma foreign_code("C", 
"

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

").

:- pragma foreign_type("C", lua_state, "lua_State *",
	[can_pass_as_mercury_type]).

:- pragma foreign_type("C", c_function, "lua_CFunction",
	[can_pass_as_mercury_type, stable]).

:- pragma foreign_decl("C", "extern void luaAP_init(lua_State *);").

:- pragma foreign_code("C", "void luaAP_init(lua_State * L) {

	/* Add tables to the registry. */
	lua_newtable(L);
	
	/* TODO: load metamethods for univ userdata */
	
	lua_setfield(L, LUA_REGISTRYINDEX, AP_USERDATA);
	
	
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

:- pragma foreign_code("C", 
"
	/* check to see if Apollo has already been initialized. */
	int luaAP_ready(lua_State * L) {
		lua_checkstack(L, 1);
		lua_getfield(L, LUA_REGISTRYINDEX, AP_READY);
		int ready = lua_toboolean(L, 1);
		lua_remove(L, 1);
		return ready;
	}
").

:- pragma foreign_proc("C", lua_ready(L::in), 
	[promise_pure, will_not_call_mercury], "
	SUCCESS_INDICATOR = luaAP_lua_ready(L);
").

% TODO Fix this:
%:- pragma foreign_proc("C", export_module(L::in, Name::in, M::in,
%	_I::di, _O::uo),
%	[promise_pure, will_not_call_mercury], 
%"
%
%	/* Push the module table onto the stack, then the module name and the
%		module itself. Assign the module, by name to the module table.*/
%	lua_getfield(L, LUA_REGISTRYINDEX, AP_MODULE);
%	lua_pushstring(L, M);
%	
%	luaAP_push(L, M, luaAP_mercury_type(M)); 
%	lua_setfield(L, -3);
%	lua_pop(L, 1);
%").

:- pragma foreign_proc("C", lua_error(L::in, Error::in),
	[promise_pure, will_not_call_mercury], "luaL_error(L, Error);").


%-----------------------------------------------------------------------------%

:- pragma foreign_type("C", var, "luaAP_Var", []) where equality is raw_equal.

:- pragma foreign_proc("C", raw_equal(A::in, B::in),
	[promise_pure, will_not_call_mercury], 
"
	lua_State * L = luaAP_var_state(A);
	lua_checkstack(L, 2);
	luaAP_push_var(L, A);	
	luaAP_push_var(L, B);
	SUCCESS_INDICATOR = lua_rawequal(L, -2, -1);
	lua_pop(2);
").

:- pred state(lua_state::out, var::in) is det.
:- func state(var) = lua_state is det.

:- pragma foreign_proc("C", state(L::out, V::in),
	[promise_pure, will_not_call_mercury],
	"L = luaAP_var_state(V);").

state(V) = L :- state(L, V).

	
:- pragma foreign_code("C",
"

/* Creates a new refrence from the stack */
luaAP_Var luaAP_new_var(lua_State * L, int index) {
	int id = luaL_ref(L, LUA_REGISTRYINDEX);
	luaAP_Var * new_var = GC_MALLOC(sizeof(luaAP_Var));
	new_var->state = L;
	new_var->id = id;
	GC_REGISTER_FINALIZER(new_var, luaAP_finalize_var, NULL, NULL);
	return new_var*;
}

/* Retreives the refrenced Lua state */
lua_State * luaAP_var_state(luaAP_Var var) {
	return var.state;
}

/* Push a refrence onto the provided stack */
void luaAP_push_var(lua_State * L, luaAP_Var var) {
	if (var.id == LUA_REFNIL) {
		lua_pushnil(L);
	}
	else if (L == var.state) {
		lua_rawgeti(L, LUA_REGISTRYINDEX, var.id);
	}
	else {
		lua_checkstack(var.state, 1);
		lua_rawgeti(var.state, LUA_REGISTRYINDEX, id);
		lua_xmove(var.state, L, 1);
	}
}

/* Remove Lua's refrence to the var in the registry */
void luaAP_finalize_var(luaAP_Var * var, void * dummy) {
	luaL_unref(state, LUA_REGISTRYINDEX, id);
}

").


:- pragma foreign_enum("C", lua_type/0, 
[
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


:- pragma foreign_proc("C", type(V::in, T::out), 
	[promise_pure, will_not_call_mercury], 
"
	lua_State * L = luaAP_var_state(V);
	luaAP_push_var(V);
	T = lua_type(L, -1);
	lua_pop(L, 1);
").

type(V) = T :- type(V, T).



%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C", nil(V::out, L::in), 
	[promise_pure, will_not_call_mercury], 
"
	lua_pushnil(L);
	V = luaAP_new_var(L);
").

nil(L) = V :- nil(V, L).

:- pragma foreign_proc("C", nil(V::in, L::out), 
	[promise_pure, will_not_call_mercury], 
"
	L = luaAP_var_state(V);
	luaAP_push_var(L, V);
	SUCCESS_INDICATOR = lua_isnil(L, -1);
	lua_pop(L, 1);
").

nil(V) :- nil(V, _).

nil = V :- nil(V).


:- pragma foreign_proc("C", int(I::in, V::out, L::in), 
	[promise_pure, will_not_call_mercury], 
"
	lua_pushinteger(L, (lua_Integer)I);
	V = luaAP_new_var(L);
").

:- pragma foreign_proc("C", int(I::out, V::in, L::out), 
	[promise_pure, will_not_call_mercury], 
"
	L = luaAP_var_state(V);
	luaAP_push_var(L, V);
	if(lua_isnumber(L, -1) {
		double number = (double)lua_tonumber(L, -1);
		I = (MR_Integer)number;
		SUCCESS_INDICATOR = !(I - number);
	} else 
		SUCCESS_INDICATOR = 0;
		
	lua_pop(L, 1);
		
").

int(I, L) = V :- int(I, V, L).
 
to_int(V, I) :- int(I, V, _).

to_int(V) = I :- to_int(V, I).

det_to_int(V, I) :- to_int(V, I) ; convert_error(V ^ state, "det_to_int", V, I).

det_to_int(V) = I :- det_to_int(V, I).


:- pragma foreign_proc("C", float(F::in, V::out, L::in), 
	[promise_pure, will_not_call_mercury], 
"
	lua_pushnumber(L, (lua_Number)F);
	V = luaAP_new_var(L);
").

:- pragma foreign_proc("C", float(F::out, V::in, L::out), 
	[promise_pure, will_not_call_mercury], 
"
	L = luaAP_var_state(V);
	luaAP_push_var(L, V);
	if(lua_isnumber(L, -1) {
		F = (MR_Float)lua_tonumber(L, -1);
		SUCCESS_INDICATOR = 1;
	} else 
		SUCCESS_INDICATOR = 0;
		
	lua_pop(L, 1);
		
").

float(F, L) = V :- float(F, V, L).

to_float(V, F) :- float(F, V, _).

to_float(V) = F :- to_float(V, F).

det_to_float(V, F) :- 
		to_float(V, F)
	 ; 
		convert_error(V ^ state, "det_to_float", V, F).

det_to_float(V) = F :- det_to_float(V, F).


:- pragma foreign_proc("C", bool(B::in, V::out, L::in), 
	[promise_pure, will_not_call_mercury], 
"
	if (B == MR_YES)
		lua_pushboolean(L, 1);
	else
		lua_pushboolean(L, 0);
	V = luaAP_new_var(L);
").

:- pragma foreign_proc("C", bool(B::out, V::in, L::out), 
	[promise_pure, will_not_call_mercury], 
"
	L = luaAP_var_state(V);
	luaAP_push_var(L, V);
	if(lua_isboolean(L, -1) {
		int boolean = lua_toboolean(L, -1);
		if(boolean) 
			B = MR_YES;
		else
			B = MR_NO;
		SUCCESS_INDICATOR = 1;
	} else 
		SUCCESS_INDICATOR = 0;
		
	lua_pop(L, 1);

").

bool(B, L) = V :- bool(B, V, L).

to_bool(V, B) :- bool(B, V, _).

to_bool(V) = B :- to_bool(V, B).

det_to_bool(V, B) :- 
		to_bool(V, B) 
	; 
		convert_error(V ^ state, "det_to_bool", V, B).

det_to_bool(V) = B :- det_to_bool(V, B).

:- pragma foreign_proc("C", string(S::in, V::out, L::in), 
	[promise_pure, will_not_call_mercury], 
"
	lua_pushstring(L, S);
	V = luaAP_new_var(L);
").

:- pragma foreign_proc("C", string(S::out, V::in, L::out), 
	[promise_pure, will_not_call_mercury], 
"
	L = luaAP_var_state(V);
	luaAP_push_var(L, V);
	if(lua_isstring(L, -1) {
		S = (MR_String)lua_tostring(L, -1);
		SUCCESS_INDICATOR = 1;
	} else 
		SUCCESS_INDICATOR = 0;
	
	lua_pop(L, 1);
").

string(S, L) = V :- string(S, V, L).

to_string(V, S) :- string(S, V, _).

to_string(V) = S :- to_string(V, S).

det_to_string(V, S) :- 
		to_string(V, S)
	; 
		convert_error(V ^ state, "det_to_string", V, S).

det_to_string(V) = S :- det_to_string(V, S).


:- pragma foreign_proc("C", c_pointer(P::in, V::out, L::in), 
	[promise_pure, will_not_call_mercury], 
"
	lua_pushlightuserdata(L, P);
	V = luaAP_new_var(L);
").

:- pragma foreign_proc("C", c_pointer(P::out, V::in, L::out), 
	[promise_pure, will_not_call_mercury], 
"
	L = luaAP_var_state(V);
	luaAP_push_var(L, V);
	if(lua_islightuserdata(L, -1) {
		P = lua_tolightuserdata(L, -1);
		
		SUCCESS_INDICATOR = 1;
	} else 
		SUCCESS_INDICATOR = 0;
		
	lua_pop(L, 1);	
").

c_pointer(P, L) = V :- c_pointer(P, V, L).

to_pointer(V, P) :- c_pointer(P, V, _).

to_pointer(V) = P :- to_pointer(V, P).

det_to_pointer(V, P) :- 
		to_pointer(V, P) 
	; 
		convert_error(V ^ state, "det_to_pointer", V, P).

det_to_pointer(V) = P :- det_to_pointer(V, P).


:- pragma foreign_proc("C", univ(U::in, V::out, L::in), 
	[promise_pure, may_call_mercury], "
	
	/* Force Mercury to hold a refrence to the univ so
	that it won't be garbage collected. */
	luaAP_intern(U);
	
	/* Create a new Lua userdata and point it to the univ */
	MR_Word * udata = lua_newuserdata(L, sizeof(MR_Word));
	udata* = U;
	
	/* Assign our new userdata a metatable */
	lua_getfield(L, LUA_REGISTRYINDEX, AP_USERDATA);
	lua_setmetatable(L, -2);
	
	V = luaAP_new_var(L);
").

:- pragma foreign_proc("C", univ(U::out, V::in, L::out), 
	[promise_pure, will_not_call_mercury], "
	L = luaAP_var_state(V);
	luaAP_push_var(L, V);
	
	MR_Word * univ = luaAP_get_univ(L)
	
	if(univ) {
		U = univ*;
		SUCCESS_INDICATOR = 1;
	} else
		SUCCESS_INDICATOR = 0;
		
	lua_pop(L, 1);

").

:- pragma foreign_code("C", " MR_Word * luaAP_get_univ(lua_State * L) {

	/* Userdata will be a valid univ value only if it's
	metatable contains a value at AP_MERCURY_UNIV. */
	
	if (luaL_getmetafield(L, -1, AP_MERCURY_UNIV)) {
		
		/* Remove the metafeild pushed by luaL_getmetafield */
		lua_pop(L, 1);
		
		/* Extract the MR_Word pointer and derefrence it */
		MR_Word * udata = (MR_Word *) lua_touserdata(L, -1);
		
		return udata;
		
	} else
		return 0;
}").
		
univ(U, L) = V :- univ(U, V, L).

to_univ(V, U) :- univ(U, V, _).

to_univ(V) = U :- to_univ(V, U).

det_to_univ(V, U) :- 
		to_univ(V, U) 
	; 
		convert_error(V ^ state, "det_to_univ", V, U).

det_to_univ(V) = U :- det_to_univ(V, U).


var(T::in, V::out, L::in) :- 
	U = univ(T),
	( U = univ(I:int) ->
		V = L ^ int(I)
	; U = univ(F:float) ->
		V = L ^ float(F)
	; U = univ(B:bool) ->
		V = L ^ bool(B)
	; U = univ(S:string) ->
		V = L ^ string(S)
	; U = univ(P:c_pointer) ->
		V = L ^ c_pointer(P)
	; 
		V = L ^ univ(U)
	).


var(T::out, V::in, L::out) :-
	some_value(V, X, L),
	( univ(Y:univ) = univ(X) ->
		univ(T) = univ(univ(Y):univ)
	;
		univ(T) = univ(x)
	).  
	
:- pragma promise_pure(var/3).

var(T, L) = V :- var(T, V, L).

from_var(V, T) :- var(T, V, _).

from_var(V) = T :- from_var(V, T).

det_from_var(V, T) :- 
		from_var(V, T) 
	; 
		convert_error(V ^ state, "det_from_var", V, T).

det_from_var(V) = T :- det_from_var(V, T).

:- some [T] pred some_value(var::in, T::out, lua_state::out) is semidet.

some_value(V, T, L) :-
	L = V ^ state,
	(I = to_int(V) ->
		T = I
	; F = to_float(V) ->
		T = F
	; B = to_bool(V) ->
		T = B
	; S = to_string(V) ->
		T = S
	; P = to_pointer(V) ->
		T = P
	; 
		T = to_univ(V)
	).


:- pred convert_error(lua_state, string, F, T).
:- mode convert_error(in, in, in,  in)  is erroneous.
:- mode convert_error(in, in, in,  out)  is erroneous.
:- mode convert_error(in, in, out, in)  is erroneous.
:- mode convert_error(in, in, out,  out)  is erroneous.

% Adapted from the body of univ.det_univ_to_type by fjh.
convert_error(L, FuncName, From, To) :- 
        FromTypeName = type_name(type_of(From)),
        ToTypeName = type_name(type_of(To)),
        string.append_list([FuncName, ": conversion failed\n",
            "\tFrom Type: ", FromTypeName, "\n",
            "\tTo Type: ", ToTypeName], ErrorString),
        lua_error(L, ErrorString).

%-----------------------------------------------------------------------------%

	% This mutvar keeps a refrence to any Mercury variable passed to Lua
	% to ensure that Mercury does not garbage collect said variable before
	% Lua is finished with it.
	%
:- mutable(reserved, map(univ, int), map.init, ground, [untrailed, attach_to_io_state]).

	% TODO: Redesign reserved system so that refrences to univs are passed
	% by integer index instead of by pointer?  Only if current implementation
	% proves to be unstable/unworkable.

:- pred intern(univ::in, io::di, io::uo) is det.

intern(U, !IO) :- 
	get_reserved(R, !IO),
	( search(R, U, I) ->
		set_reserved(det_update(R, U, I + 1), !IO) 
	;
		set_reserved(det_insert(R, U, 1), !IO)
	).

:- pragma foreign_export("C", intern(in, di, uo), "luaAP_intern").

:- pred release(lua_state::in, univ::in, io::di, io::uo) is det.


release(L, U, !IO) :- get_reserved(R, !IO),
	( search(R, U, I) -> 
		( I > 1 -> 
			set_reserved(det_update(R, U, I - 1), !IO)
		;
			set_reserved(delete(R, U), !IO)
		)
	;
		lua_error(L, 
			string.append_list(
			["Attempted to release a Mercury type: ",
			type_name(univ_type(U)),
			" that was not interned."])
		)
	).



:- pragma foreign_export("C", release(in, in, di, uo), "luaAP_release").

%-----------------------------------------------------------------------------%


:- type mr_function ---> some [T] (mr_function(T) => function(T)).

function(L, T, V) :- 
	F = 'new mr_function'(T), 
	U = (L ^ var(univ(F:mr_function))), 
	new_function(L, U, V).

function(L, T) = V :- function(L, T, V).

:- pred new_function(lua_state::in, var::in, var::out).

:- pragma foreign_proc("C", new_function(L::in, F::in, V::out), 
	[promise_pure, will_not_call_mercury], 
"
	luaAP_push_var(L, F);
	lua_pushclosure(L, luaAP_call, 1);
	V = luaAP_new_var(L);
").


:- func lua_call_function(lua_state::in, io::di, io::uo) = (int::out) is det.

lua_call_function(L, !IO) = Num :-
	MF = mr_function(F),	
	(
		 get_function_upvalue(L, univ(MF), !IO) 
	; 
		lua_error(L, "lua_call_function: Failed to load function") 
	),
	A = args(L),
	call_function(F, A, R, !IO),
	Return = return(L, R),
	require_complete_switch [Num]
	( Return = nil ->
		Num = 0
	; Return = return_var(V:var) ->
		push_var(L, V, !IO),
		Num = 1
	; 
		Return = return_list(V:list(var)),
		push_list(L, V, Num, !IO)
	).
	
:- pragma foreign_export("C", 
	lua_call_function(in, di, uo) = out, "luaAP_call").
		

:- pred get_function_upvalue(lua_state::in, univ::out, io::di, io::uo) is det.

:- pragma foreign_proc("C", 
	get_function_upvalue(L::in, U::out, _I::di, _O::uo),
	[promise_pure, will_not_call_mercury], 
"
	lua_pushvalue(L, lua_upvalueindex(AP_FUNCTION_UPVALUE));
	
	MR_Word * univ = luaAP_get_univ(L);
	lua_pop(L, 1);
	
	assert(univ);
	
	U = univ*;
").
	

:- pred push_var(lua_state::in, var::in, io::di, io::uo) is det.

:- pragma foreign_proc("C", push_var(L::in, V::in, _I::di, _O::uo),
	[promise_pure, will_not_call_mercury], 
"
	luaAP_push_var(L, V);").
	
:- pred push_list(lua_state::in, list(var)::in, int::out, io::di, io::uo)
	is det.

push_list(_, [], 0, !IO).

push_list(L, [V | M], Num + 1, !IO) :- 
	push_var(L, V, !IO), 
	push_list(L, M, Num, !IO).

:- instance args(lua_state) where [ args(L) = L ].

:- instance return(return) where [ return(_, R) = R ].





