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
:- pred lua.init(lua_state::in, io::di, io::uo) is det.

	% Verify that Lua is prepared for interaction with Mercury
	%
:- pred ready(lua_state::in) is semidet.


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


:- type var
	---> 	nil
	;	int(int)
	;	float(float)
	;	bool(bool)
	;	string(string)
	;	c_pointer(c_pointer)
	;	univ_var(univ)

	where equality is var_equals.


;	lightuserdata
	;	number
	;	string
	;	table
	;	function
	;	userdata
	;	thread.

:- pred var(T::in, var::out) is det.

:- func var(T) = var is det.

:- func table(T) = var is det <= table(T).
:- func function(T) = var is det <= function(T).
:- func userdata(T) = var is det <= userdata(T).


:- pred var_equals(var::in, var::in) is semidet.

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

:- pred lua_type(var::in, lua_type::out) is det.
:- func lua_type(var) = lua_type.

%-----------------------------------------------------------------------------%

	% Represents a refrence to a variable instantiated in Lua.
	%
:- type ref.

:- pred ref_state(ref::in, lua_state::out) is det.
:- func ref_state(ref) = lua_state is det.



:- pred ref_equals(ref::in, ref::in) is semidet.


:- pred ref_type(ref::in, lua_type::out) is det.
:- func ref_type(ref) = lua_type is det.



%-----------------------------------------------------------------------------%

% TODO: Tables

:- typeclass table(T) where [].

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
	

:- typeclass userdata(T) where [].


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

#define AP_MODULE ""luaAPOLLO_LANDER_MODULE""
#define AP_READY ""luaAPOLLO_LANDER_READY""
#define AP_LOCKED ""luaAPOLLO_LANDER_LOCKED""

#define AP_TYPE ""__mercury_type""

#define AP_UNIV ""luaAPOLLO_LANDER_MERCURY_UNIV""
#define AP_FUNCTION ""luaAPOLLO_LANDER_MERCURY_FUNCTION""
#define AP_USERDATA ""luaAPOLLO_LANDER_MERCURY_USERDATA""


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

	/* univ_var(univ) metatable */
	lua_newtable(L);
	
	lua_pushboolean(L, 1);
	lua_setfield(L, -1, AP_IS_UNIV);

	/* TODO: load metamethods for univ vars */
	
	lua_setfield(L, LUA_REGISTRYINDEX, AP_UNIV);

	/* function(univ) metatable */
	lua_newtable(L);

	lua_pushboolean(L, 1);
	lua_setfield(L, -1, AP_IS_FUNCTION);
	
	/* TODO: load metamethods for function vars */
	
	lua_setfield(L, LUA_REGISTRYINDEX, AP_FUNCTION);

	/* univ_var(univ) metatable */
	lua_newtable(L);

	lua_pushboolean(L, 1);
	lua_setfield(L, -1, AP_IS_USERDATA);
	
	/* TODO: load metamethods for userdata vars */
	
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

:- pragma terminates(lua_error/2).


%-----------------------------------------------------------------------------%


var(T, V) :- 
	( univ(T) = univ(T1:int) ->
		V = int(T1) 
	; univ(T) = univ(T1:float) ->
		V = float(T1)
	; univ(T) = univ(T1:bool) ->
		V = bool(T1)
	; univ(T) = univ(T1:string) ->
		V = string(T1)
	; univ(T) = univ(T1:c_pointer) ->
		V = c_pointer(T1)
	; univ(T) = univ(T1:ref) ->
		V = ref(T1)
	; univ(T) = univ(T1:univ) ->
		V = univ_var(T1)
	;
		U = univ(T),
		V = univ_var(U)
	).

var(T) = V :- var(T, V).



table(T) = 'new table_var'(T).
function(F) = 'new function_var'(F).
userdata(U) = 'new userdata_var'(U).


:- pred var_to_ref(lua_state, var, ref).
:- mode var_to_ref(in, in, out) is det.
:- mode var_to_ref(out, out, in) is semidet.

var_to_ref(L, nil, 	R) 	:- nil_ref(L, R).
var_to_ref(L, int(I), 	R) 	:- int_to_ref(L, I, R).
var_to_ref(L, float(F), R) 	:- float_to_ref(L, F, R).
var_to_ref(L, bool(B),	R) 	:- bool_to_ref(L, B, R).
var_to_ref(L, string(S), R) 	:- string_to_ref(L, S, R).
var_to_ref(L, c_pointer(P), R) 	:- c_pointer_to_ref(L, P, R).
var_to_ref(L, ref(R), 	R)	:- ref_state(R, L).
var_to_ref(L, U@univ_var(_), R)	:- univ_to_ref(L, U, R).
var_to_ref(L, T@'new table'(_), R) :- table_to_ref(L, T, R).
var_to_ref(L, F@'new function'(_), R) :- function_to_ref(L, F, R).
var_to_ref(L, U@'new userdata'(_), R) :- userdata_to_ref(L, U, R).




var_equals(nil, nil).
var_equals(int(I), int(I)).
var_equals(float(F), float(F)).
var_equals(int(I), float(float(I))).
var_equals(float(float(I)), int(I)).
var_equals(bool(B), bool(B)).
var_equals(string(S), string(S)).
var_equals(c_pointer(P), c_pointer(P)).
var_equals(univ_var(U), univ(U)).
var_equals(V1, V2) :- var_to_univ(V1, U1), var_to_univ(V2, U2), U1 = U2.
var_equals(int(I), ref(R)) :- int_to_ref(ref_state(R), I, R).
var_equals(float(F), ref(R)) :- float_to_ref(ref_state(R), F, R).
var_equals(bool(B), ref(R)) :- bool_to_ref(ref_state(R), B, R).
var_equals(string(S), ref(R)) :- string_to_ref(ref_state(R), S, R).
var_equals(c_pointer(P), ref(R)) :- c_pointer_to_ref(ref_state(R), P, R).
var_equals(U@univ(_), ref(R)) :- var_to_ref(ref_state(R), U , R).
var_equals(T@'new table'(_), ref(R)) :- var_to_ref(ref_state(R), T , R).
var_equals(F@'new function'(_), ref(R)) :- 
	function_to_ref(ref_state(R), F , R).
var_equals(U@'new userdata'(_), ref(R)) :- var_to_ref(ref_state(R), U , R).



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


lua_type(nil, nil).
lua_type(int(_), number).
lua_type(float(_), number).
lua_type(bool(_), boolean).
lua_type(string(_), string).
lua_type(c_pointer(_), lightuserdata).
lua_type(univ_var(_), userdata).
lua_type(ref(R), ref_type(R)). 
lua_type('new lua_var'(_), userdata).
lua_type('new table'(_), table).
lua_type('new function'(_), function).
lua_type('new userdata'(_), userdata).

lua_type(V) = T :- lua_type(V, T).

%-----------------------------------------------------------------------------%


:- pragma foreign_type("C", ref, "luaAP_Ref *", [can_pass_as_mercury_type])
	where equality is ref_equals.

:- pragma foreign_proc("C", ref_equals(A::in, B::in),
	[promise_pure, will_not_call_mercury], 
"
	lua_State * L = luaAP_ref_state(A);
	lua_checkstack(L, 2);
	luaAP_push_ref(L, A);	
	luaAP_push_ref(L, B);
	SUCCESS_INDICATOR = lua_rawequal(L, -2, -1);
	lua_pop(2);
").



:- pragma foreign_proc("C", ref_state(Ref::in, L::out) ,
	[promise_pure, will_not_call_mercury],
	"L = luaAP_ref_state(Ref);").

ref_state(R) = L :- ref_state(R, L).
	
:- pragma foreign_code("C",
"

typedef struct luaAP_Ref {
	lua_State * state;
	int id;
}


/* Creates a new refrence from the stack */
luaAP_Ref * luaAP_new_ref(lua_State * L, int index) {
	int id = luaL_ref(L, LUA_REGISTRYINDEX);
	luaAP_Ref * new_ref = MR_GC_NEW(luaAP_Ref);
	new_ref->state = L;
	new_ref->id = id;
	MR_GC_register_finalizer(new_ref, luaAP_finalize_ref);
	return new_var;
}

/* Retreives the refrenced Lua state */
lua_State * luaAP_ref_state(luaAP_Ref * ref) {
	return ref->state;
}

/* Push a refrence onto the provided stack */
void luaAP_push_ref(lua_State * L, luaAP_Ref * ref) {
	lua_State * L0 = ref-> state;	
	int id = ref->id;
	if (id == LUA_REFNIL) {
		lua_pushnil(L);
	}
	else if (L == L0) {
		lua_rawgeti(L, LUA_REGISTRYINDEX, id);
	}
	else {
		lua_checkstack(L0, 1);
		lua_rawgeti(L0, LUA_REGISTRYINDEX, id);
		lua_xmove(L0, L, 1);
	}
}

/* Remove Lua's refrence to the var in the registry */
void luaAP_finalize_ref(luaAP_Ref * ref, void * dummy) {
	luaL_unref(ref->state, LUA_REGISTRYINDEX, ref->id);
}

"). 


:- pragma foreign_proc("C", ref_type(Ref::in, Type::out), 
	[promise_pure, will_not_call_mercury], 
"
	lua_State * L = luaAP_ref_state(Ref);
	luaAP_push_ref(Ref);
	Type = lua_type(L, -1);
	lua_pop(L, 1);
").

ref_type(R) = T :- ref_type(R, T).

%-----------------------------------------------------------------------------%


:- pred nil_ref(lua_state, ref).
:- mode nil_ref(in, out).
:- mode nil_ref(out, in).

:- pragma foreign_proc("C", nil_ref(L::in, Ref::out), 
	[promise_pure, will_not_call_mercury], 
"
	lua_pushnil(L);
	Ref = luaAP_new_ref(L);
").


:- pragma foreign_proc("C", nil_ref(L::out, Ref::in), 
	[promise_pure, will_not_call_mercury], 
"
	L = luaAP_ref_state(Ref);
	luaAP_push_ref(L, Ref);
	SUCCESS_INDICATOR = lua_isnil(L, -1);
	lua_pop(L, 1);
").



:- pred int_to_ref(lua_state, int, ref).
:- mode int_to_ref(in, in, out).
:- mode int_to_ref(out, out, in).

:- pragma foreign_proc("C", int_to_ref(L::in, Int::in, Ref::out), 
	[promise_pure, will_not_call_mercury], 
"
	lua_pushinteger(L, (lua_Integer)Int);
	Ref = luaAP_new_ref(L);
").

:- pragma foreign_proc("C", int_to_ref(L::out, Int::out, Ref::in), 
	[promise_pure, will_not_call_mercury], 
"
	L = luaAP_ref_state(Ref);
	luaAP_push_ref(L, Ref);
	if(lua_isnumber(L, -1) {
		double number = (double)lua_tonumber(L, -1);
		I = (MR_Integer)number;
		SUCCESS_INDICATOR = !(Int - number);
	} else 
		SUCCESS_INDICATOR = 0;
		
	lua_pop(L, 1);
		
").


:- pred float_to_ref(lua_state, float, ref).
:- mode float_to_ref(in, in, out).
:- mode float_to_ref(out, out, in).


:- pragma foreign_proc("C", float_to_ref(L::in, Float::in, Var::out), 
	[promise_pure, will_not_call_mercury], 
"
	lua_pushnumber(L, (lua_Number)Float);
	Var = luaAP_new_ref(L);
").

:- pragma foreign_proc("C", float_to_ref(L::out, Float::out, Var::in), 
	[promise_pure, will_not_call_mercury], 
"
	L = luaAP_ref_state(Var);
	luaAP_push_ref(L, Var);
	if(lua_isnumber(L, -1) {
		Float = (MR_Float)lua_tonumber(L, -1);
		SUCCESS_INDICATOR = 1;
	} else 
		SUCCESS_INDICATOR = 0;
		
	lua_pop(L, 1);
		
").

:- pred bool_to_ref(lua_state, bool, ref).
:- mode bool_to_ref(in, in, out).
:- mode bool_to_ref(out, out, in).


:- pragma foreign_proc("C", bool_to_ref(L::in, Bool::in, Var::out), 
	[promise_pure, will_not_call_mercury], 
"
	if (Bool == MR_YES)
		lua_pushboolean(L, 1);
	else
		lua_pushboolean(L, 0);
	Var = luaAP_new_ref(L);
").

:- pragma foreign_proc("C", bool_to_ref(L::out, Bool::out, Var::in), 
	[promise_pure, will_not_call_mercury], 
"
	L = luaAP_ref_state(Var);
	luaAP_push_ref(L, Var);
	if(lua_isboolean(L, -1) {
		int boolean = lua_toboolean(L, -1);
		if(boolean) 
			Bool = MR_YES;
		else
			Bool = MR_NO;
		SUCCESS_INDICATOR = 1;
	} else 
		SUCCESS_INDICATOR = 0;
		
	lua_pop(L, 1);

").

:- pred string_to_ref(lua_state, string, ref).
:- mode string_to_ref(in, in, out).
:- mode string_to_ref(out, out, in).

:- pragma foreign_proc("C", string_to_ref(L::in, String::in, Var::out), 
	[promise_pure, will_not_call_mercury], 
"
	lua_pushstring(L, String);
	Var = luaAP_new_ref(L);
").

:- pragma foreign_proc("C", string_to_ref(L::out, String::out, Var::in), 
	[promise_pure, will_not_call_mercury], 
"
	L = luaAP_ref_state(Var);
	luaAP_push_ref(L, Var);
	if(lua_isstring(L, -1) {
		String = (MR_String)lua_tostring(L, -1);
		SUCCESS_INDICATOR = 1;
	} else 
		SUCCESS_INDICATOR = 0;
	
	lua_pop(L, 1);
").

:- pred c_pointer_to_ref(lua_state, c_pointer, ref).
:- mode c_pointer_to_ref(in, in, out).
:- mode c_pointer_to_ref(out, out, in).


:- pragma foreign_proc("C", c_pointer_to_ref(L::in, Pointer::in, Var::out), 
	[promise_pure, will_not_call_mercury], 
"
	lua_pushlightuserdata(L, Pointer);
	Var = luaAP_new_ref(L);
").

:- pragma foreign_proc("C", c_pointer_to_ref(L::out, Pointer::out, Var::in), 
	[promise_pure, will_not_call_mercury], 
"
	L = luaAP_ref_state(Var);
	luaAP_push_ref(L, Var);
	if(lua_islightuserdata(L, -1) {
		Pointer = lua_tolightuserdata(L, -1);
		
		SUCCESS_INDICATOR = 1;
	} else 
		SUCCESS_INDICATOR = 0;
		
	lua_pop(L, 1);	
").

%-----------------------------------------------------------------------------%

:- pred univ_to_ref(lua_state, var, ref).
:- mode univ_to_ref(in, in, out) is semidet.
:- mode univ_to_ref(out, out, in) is semidet.

univ_to_ref(L, Var1, Ref) :-
	Var1 = lua_univ(U1),
	Var2 = lua_univ(U2),
	univ(U1) = univ(U2),
	univ(Var1) = univ(Var2),
	univ_to_ref_unsafe(L, Var2, Ref).
	
		
:- pred univ_to_ref_unsafe(lua_state::in, var::in, ref::out) is det.

:- pragma foreign_proc("C", univ_to_ref_unsafe(L::in, Var::in, Ref::out), 
	[promise_pure, may_call_mercury], 
	"Ref = *luaAP_get_ref(L, Var, AP_UNIV);").	

:- pragma foreign_proc("C", univ_to_ref_unsafe(L::out, Var::out, Ref::in), 
	[promise_pure, will_not_call_mercury], "
	L = luaAP_ref_state(Ref);
	luaAP_push_ref(L, Ref);
	
	Var = luaAP_get_var(L, AP_UNIV);
	
	if(Var)
		SUCCESS_INDICATOR = 1;
	else
		SUCCESS_INDICATOR = 0;
		
	lua_pop(L, 1);

").

%-----------------------------------------------------------------------------%

:- pred table_to_ref(lua_state, var, ref).
:- mode table_to_ref(in, in, out) is semidet.
:- mode table_to_ref(out, out, in) is semidet.

table_to_ref(L, Var1, Ref) :-
	Var1 = table(U1),
	Var2 = table(U2),
	univ(U1) = univ(U2),
	univ(Var1) = univ(Var2),
	userdata_to_ref_unsafe(L, Var2, Ref).
	
		
:- pred userdata_to_ref_unsafe(lua_state::in, var::in, ref::out) is det.

:- pragma foreign_proc("C", userdata_to_ref_unsafe(L::in, Var::in, Ref::out), 
	[promise_pure, may_call_mercury], 
	"Ref = *luaAP_get_ref(L, Var, AP_USERDATA);").	

:- pragma foreign_proc("C", userdata_to_ref_unsafe(L::out, Var::out, Ref::in), 
	[promise_pure, will_not_call_mercury], "
	L = luaAP_ref_state(Ref);
	luaAP_push_ref(L, Ref);
	
	Var = luaAP_get_var(L, AP_UNIV);
	
	if(Var)
		SUCCESS_INDICATOR = 1;
	else
		SUCCESS_INDICATOR = 0;
		
	lua_pop(L, 1);

").

%-----------------------------------------------------------------------------%

:- pred userdata_to_ref(lua_state, var, ref).
:- mode userdata_to_ref(in, in, out) is semidet.
:- mode userdata_to_ref(out, out, in) is semidet.

userdata_to_ref(L, Var1, Ref) :-
	Var1 = userdata(U1),
	Var2 = userdata(U2),
	univ(U1) = univ(U2),
	univ(Var1) = univ(Var2),
	userdata_to_ref_unsafe(L, Var2, Ref).
	
		
:- pred userdata_to_ref_unsafe(lua_state::in, var::in, ref::out) is det.

:- pragma foreign_proc("C", userdata_to_ref_unsafe(L::in, Var::in, Ref::out), 
	[promise_pure, may_call_mercury], 
	"Ref = *luaAP_get_ref(L, Var, AP_USERDATA);").	

:- pragma foreign_proc("C", userdata_to_ref_unsafe(L::out, Var::out, Ref::in), 
	[promise_pure, will_not_call_mercury], "
	L = luaAP_ref_state(Ref);
	luaAP_push_ref(L, Ref);
	
	Var = luaAP_get_var(L, AP_UNIV);
	
	if(Var)
		SUCCESS_INDICATOR = 1;
	else
		SUCCESS_INDICATOR = 0;
		
	lua_pop(L, 1);

").

%-----------------------------------------------------------------------------%


:- pred function_to_ref(lua_state, var, ref).
:- mode function_to_ref(in, in, out) is semidet.
:- mode function_to_ref(out, out, in) is semidet.

univ_to_ref(L::in, Var1::in, Ref::out) :-
	Var1 = function(U1),
	Var2 = function(U2),
	univ(U1) = univ(U2),
	univ(Var1) = univ(Var2),
	function_to_ref_unsafe(L, Var2, Ref0),
	make_closure(L, Ref0, Ref).

univ_to_ref(L::out, Var1::out, Ref::in) :-
	Var1 = function(U1),
	Var2 = function(U2),
	univ(U1) = univ(U2),
	univ(Var1) = univ(Var2),
	function_to_ref_unsafe(L, Var2, Ref).
	
		
:- pred function_to_ref_unsafe(lua_state::in, var::in, ref::out) is det.

:- pragma foreign_proc("C", function_to_ref_unsafe(L::in, Var::in, Ref::out), 
	[promise_pure, may_call_mercury], 
	"Ref = *luaAP_get_ref(L, Var, AP_UNIV);").	

:- pragma foreign_proc("C", function_to_ref_unsafe(L::out, Var::out, Ref::in), 
	[promise_pure, will_not_call_mercury], "
	L = luaAP_ref_state(Ref);
	luaAP_push_ref(L, Ref);
	
	Var = luaAP_get_var(L, AP_UNIV);
	
	if(Var)
		SUCCESS_INDICATOR = 1;
	else
		SUCCESS_INDICATOR = 0;
		
	lua_pop(L, 1);

").




	

:- pred make_closure(lua_state::in, ref::in, ref::out).
:- pragma foreign_proc("C", make_closure(L::in, Function::in, Ref::out), 
	[promise_pure, will_not_call_mercury], 
"
	luaAP_push_ref(L, Function);
	lua_pushclosure(L, luaAP_call, 1);
	Ref = luaAP_new_ref(L);
").

:- func lua_call_function(lua_state::in, io::di, io::uo) = (int::out) is det.

lua_call_function(L, !IO) = Num :-
	NewFunction = function(Function),	
	(
		 get_function_upvalue(L, univ(NewFunction), !IO) 
	; 
		lua_error(L, "lua_call_function: Failed to load function") 
	),
	Args = args(L),
	call_function(Function, Args, Return, !IO),
	ReturnValue = return(L, Return),
	require_complete_switch [ReturnValue]
	( ReturnValue = nil ->
		Num = 0
	; ReturnValue = return_var(Var) ->
		push_var(L, Var, !IO),
		Num = 1
	; 
		ReturnValue = return_list(VarList),
		push_list(L, VarList, Num, !IO)
	).

:- pragma foreign_export("C", 
	lua_call_function(in, di, uo) = out, "luaAP_call").
		

:- pred get_function_upvalue(lua_state::in, univ::out, io::di, io::uo) is det.

:- pragma foreign_proc("C", 
	get_function_upvalue(L::in, Univ::out, _I::di, _O::uo),
	[promise_pure, will_not_call_mercury], 
"
	lua_pushvalue(L, lua_upvalueindex(AP_FUNCTION_UPVALUE));
	
	MR_Word * new_univ = luaAP_get_var(L);
	lua_pop(L, 1);
	
	assert(new_univ);
	
	Univ = new_univ*;
").
	

:- pred push_var(lua_state::in, var::in, io::di, io::uo) is det.

:- pragma foreign_proc("C", push_var(L::in, Var::in, _I::di, _O::uo),
	[promise_pure, will_not_call_mercury], 
"
	luaAP_push_ref(L, Var);").
	
:- pred push_list(lua_state::in, list(var)::in, int::out, io::di, io::uo)
	is det.

push_list(_, [], 0, !IO).

push_list(L, [Var | Rest], Num + 1, !IO) :- 
	push_var(L, Var, !IO), 
	push_list(L, Rest, Num, !IO).

:- instance args(lua_state) where [ args(L) = L ].

:- instance return(return) where [ return(_, Return) = Return ].

%-----------------------------------------------------------------------------%


%:- pred var_to_ref(lua_state, var, ref).
%:- mode var_to_ref(in, in, out).
%:- mode var_to_ref(out, out, in).




:- pragma foreign_code("C", 
" 
luaAP_Ref luaAP_get_ref(lua_State * L, MR_Word var, const char * var_type) {

	/* Force Mercury to hold a refrence to the var so
	that it won't be garbage collected. */
	luaAP_intern(Var);
	
	/* Create a new Lua userdata and point it to the var */
	MR_Word * udata = lua_newuserdata(L, sizeof(MR_Word));
	*udata = var;
	
	/* Assign our new userdata a metatable */
	lua_getfield(L, LUA_REGISTRYINDEX, var_type);
	lua_setmetatable(L, -2);
	
	return luaAP_new_ref(L);
}


MR_Word * luaAP_get_var(lua_State * L, const char * var_type) {


	/* Userdata will be a valid univ value only if it's
	metatable contains a value at AP_MERCURY_UNIV. */
	
	if (luaL_getmetafield(L, -1, AP_TYPE)) {

		if(lua_tostring(L, -1) = var_type) {
		
			/* Remove the metafeild pushed by luaL_getmetafield */
			lua_pop(L, 1);
		
			/* Extract the MR_Word pointer */
			MR_Word * var = (MR_Word *) lua_touserdata(L, -1);
		
			return var;
		}
		else
			lua_pop(L, 1);
	}

	return 0;
} ").
		
%-----------------------------------------------------------------------------%

	% This mutvar keeps a refrence to any Mercury variable passed to Lua
	% to ensure that Mercury does not garbage collect said variable before
	% Lua is finished with it.
	%
:- mutable(reserved, map(var, int), map.init, ground, [untrailed, attach_to_io_state]).

	% TODO: Redesign reserved system so that refrences to univs are passed
	% by integer index instead of by pointer?  Only if current implementation
	% proves to be unstable/unworkable.

:- pred intern(var::in, io::di, io::uo) is det.

intern(V, !IO) :- 
	get_reserved(R, !IO),
	( search(R, V, I) ->
		set_reserved(det_update(R, V, I + 1), !IO) 
	;
		set_reserved(det_insert(R, V, 1), !IO)
	).

:- pragma foreign_export("C", intern(in, di, uo), "luaAP_intern").

:- pred release(lua_state::in, var::in, io::di, io::uo) is det.


release(L, V, !IO) :- get_reserved(R, !IO),
	( search(R, V, I) -> 
		( I > 1 -> 
			set_reserved(det_update(R, V, I - 1), !IO)
		;
			set_reserved(delete(R, V), !IO)
		)
	;
		lua_error(L, 
			string.append_list(
			["Attempted to release a Mercury type: ",
			type_name(type_of(V)),
			" that was not interned."])
		)
	).



:- pragma foreign_export("C", release(in, in, di, uo), "luaAP_release").

%-----------------------------------------------------------------------------%


