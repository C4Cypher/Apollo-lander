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

:- use_module io.
:- import_module int.
:- import_module float.
:- import_module bool.
:- import_module string.
:- import_module list.
:- import_module univ.

:- type io == io.state.

%-----------------------------------------------------------------------------%
%
% The Lua state
%

	% The state type is a refrence to the Lua state, also known as the
	% Lua Virtual Machine.  This type is defined in lua.h as
	% the C type "lua_State *". Note that as a convention borrowed from 
	% the C API, operations that query or manipulate the Lua state will
	% use the variable term 'L' to refer to the Lua state.
	%
:- type lua.state.

	% init(L, !IO).
	% Prepares a lua_State for interaction with Mercury
	%
:- pred lua.init(lua_state::in, io::di, io::uo) is det.

	%
	% Verify that Lua is prepared for interaction with Mercury
	%
:- pred lua.ready(lua_state::in) is semidet.



%-----------------------------------------------------------------------------%
%
% Variables
%

% A Lua variable can be used to store any value that can be stored as a 
% C type.  Furthermore, because variables are instantiated and stored within
% the Lua state, Mercury cannot construct, deconstruct, equality test or 
% refrence Lua variables directly like it can C types. These operations are
% handled by the C API.
%
% Another thing to note is the fact that the lifetimes of Lua variables are
% manged by the Lua garbage collector, a mark and sweep collector not unlike
% the Bohem GC. 
%
% The current implementation of this library places reservations on and
% registers finalizer callbacks with variables passed by refrence between
% Mercury and Lua in such a manner that a Variable passed to it's non-native 
% environment will not be collected by it's own garbage collector until the
% non-native garbage collector calls the finalizer associated with it.

 
	% The var type represents an indirect refrence to a variable 
	% instantiated in Lua. So long as Mercury does not garbage collect 
	% the var, Lua will not garbage collect the refrenced variable.
	%
:- type lua.var.

%-----------------------------------------------------------------------------%
%
% Passing data between Mercury and Lua 
%	

% Mercury has no equivalent to the Lua nil value.  In Lua, nil does represent
% the empty list, but the abscence of value, similar to the SQL NULL value.

	% is_nil(Var).
	% Check to see if Var is a nil value. 
	%
:- pred is_nil(var::in) is semidet.

	% push_nil(L, Var) <=> push_nil(L) = Var.
	% Create a new Lua variable with a value of nil.
	%
:- pred push_nil(state::in, var::out) is det.
:- func push_nil(state) = var is det.


% The following procedures all carry the same type and mode signatures, and
% all have the same purpose, but for different types.
%
% to_Type(Var, Type) <=> to_Type(Var) = Type.
% These will accept a Lua variable and return the associated Mercury variable 
% of the corresponding Type, failing if the type of the Lua variable is not
% convertable to Type.
%
% push_Type(Type, Var) <=> push_Type(Type) = Var.
% These will accept the corresponding Mercury Type and return a Lua variable
% storing that Type.  If Lua cannot allocate the memory for a new variable,
% it will abort.
%
% Note that when passing numeric values, Lua will implicitly cast strings
% to numbers and back if the string represents a numeric value. 
% For example, in Lua, "3" == 3 will evaluate to true.

	% Mercury int values will be converted to floats before being stored
	% as Lua numbers.
	%
	% push_int will fail when attempting to pass a number that has a 
	% non-zero fraction part. 
	%
:- pred to_int(var::in, int::out) is semidet.
:- func to_int(var) = int is semidet.

:- pred push_int(state::in, int::in, var::out) is det.
:- func push_int(state, int) = var is det.


	% Both Mercury floats and Lua numbers are represented in C by a
	% double precision floating point value, as a result, to_float
	% will never fail if the Lua variable is garunteed to be a number. 
	%
:- pred to_float(var::in, float::out) is semidet.
:- func to_float(var) = float is semidet.

:- pred push_float(state::in, float::in, var::out) is det.
:- func push_float(state, float) = var is det.


% Lua boolean values differently than C does. In Lua, any value that is not
% false or nil will evaluate to 'true' when Lua expects a boolean value. 

:- pred to_bool(var::in, bool::out) is semidet.
:- func to_bool(var) = bool is semidet.

	% explicit_bool will test the type of the Lua variable, failing if
	% it is any Lua type other than boolean.
	%
:- pred explicit_bool(var::in, bool::out) is semidet.
:- func explicit_bool(var) = bool is semidet.

:- pred push_bool(state::in, bool::in, var::out) is det.
:- func push_bool(state, bool) = var is det.


% Lua strings behave similarly to Mercury strings and Char * pointers.
% The only notable difference is the fact that Lua internalizes any new
% string that isn't already instantiated in Lua.  If two strings are equal in
% Lua, then behind the scenes, they are refrencing the same address.  This 
% makes table lookups and equality tests very efficient, at the cost of making
% string concatenation VERY expensive.
%
% Because of this, a string value is not garbage collected by Lua until all
% string variables with that value are no longer in use.  This allows some
% interesting tricks for managing the lifetime of variables (especially for
% memoizing) through the use of string keys in weak tables.
 
:- pred to_string(var::in, string::out) is semidet.
:- func to_string(var) = string is semidet.

:- pred push_string(state::in, string::in, var::out) is det.
:- func push_string(state, string) = var is det.


% C pointer types can be passed to Lua using a special variation of the
% Lua userdata type called lightuserdata.  Unlike full userdata, pointers
% passed as lightuserdata cannot be assigned metatables, and are not 
% managed by the Lua garbage collector.  As a result, be careful when passing
% pointers as lightuserdata that might be garbage collected by Mercury.

:- pred to_pointer(var::in, c_pointer::out) is semidet.
:- func to_pointer(var) = c_pointer is semidet.

:- pred push_pointer(state::in, c_pointer::in, var::out) is det.
:- func push_pointer(state, c_pointer) = var is det.


% In Lua, the only native data-structure is the table.  I'table', a refrence to an associative array, used for lists and maps.
% Unlike an assoc_list, a Lua table may not associate more than
% one value with any given key, and thus, behaves more like the
% Mercury map type.

:- pred to_table(var::in, table::out) is semidet.
:- func to_table(var) = table is semidet.

:- pred push_table(state::in, table::in, var::out) is det.
:- func push_table(state, table) = var is det.


% Lua functions are impure, lexically scoped, may have a variable number of
% arguments and return values, can be curried, can be passed freely as first
% class variables.  For the sake of stability and sanity, Lua functions may
% not be invoked directly from Mercury using this Library, only passed as
% variables.

:- pred to_function(var::in, function::out) is semidet.
:- func to_function(var) = function is semidet.

:- pred push_function(state::in, function::in, var::out) is det.
:- func push_function(state, function) = var is det.


	% coroutine passing

:- pred to_thread(var::in, state::out) is semidet.
:- func to_thread(var) = state is semidet.

:- pred push_thread(state::in, state::in, var::out) is det.
:- func push_thread(state, state) = var is det.


	% userdata passing

:- some [T] pred from_userdata(var::in, T::out) is semidet.
:- some [T] func from_userdata(var) =  is semidet.

:- pred push_userdata(state::in, T::in, var::out) is det.
:- func push_userdata(state, T) = var is det.

:- pred push_userdata(state::in, table::in, T::in, var::out) is det.
:- func push_userdata(state, table, T) = var is det.


:- pred ref_state(ref::in, lua_state::out) is det.
:- func ref_state(ref) = lua_state is det.

:- pred var_equals(var::in, var::in) is semidet.

%-----------------------------------------------------------------------------%


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

:- pred lua.type(var::in, lua_type::out) is det.
:- func lua.type(var) = lua_type.

%-----------------------------------------------------------------------------%
%
% Lua tables
%

	% Represents a refrence to a Lua table.  Note that for the purposes
	% of this library, values may only be assigned to unique tables,
	% created in Mercury.  Any tables that have been exposed to Lua
	% execution must be considered immutable to preserve Mercury's
	% pure declarative semantics.
	%
	% This type is identical to the var type, but with a garuntee that
	% the value held by table is, in fact, a Lua table.
	%
:- type lua.table.

	% new_table(L, New_Table)
	% Create a new, unique, empty table.
	%
:- pred new_table(state::in, table::uo) is det.
:- func new_table(state) = table.
:- mode new_table(in) = uo is det.

	% new_table(L, Metatable, New_table)
	% Create a new table and assign a metatable to it
	%
:- pred new_table(state::in, table::in, table::uo) is det.
:- func new_table(state, table) = table.
:- mode new_table(in, in) = uo is det.

	% get(Table, Key, Value)
	% Perform a raw lookup on a Lua table.
	% Wil always return nil on lookups to nil keys.
	%
:- pred get(table, var, var).
:- mode get(in, in, out) is det.
:- mode get(in, out, out) is nondet.
:- func get(table, var) = var is det.


	% set(Table0, Table, Key, Value)
	% Perform a raw destructive update on a unique table.
	% Will ignore attempts to assign to nil key. 
	%
:- pred set(table::di, table::uo, var::in, var::in) is det.
:- func set(table, var, var) = table.
:- mode set(ui, in, in) = uo is det.

%-----------------------------------------------------------------------------%
	
% Lua functions are either compiled chunks of Lua source code, or are C
% function pointers as defined by the C type "lua_CFunction". Lua functions 
% are varadic, accepting variable numbers of arguments and
% return values.
% 
% Lua handles functions as first-class variables, they can be assigned
% to variables and passed as function arguments or return values in
% the same manner as any other value in Lua.
% 
% Lua functions are inherently impure.  Not only can they cause
% side effects to variables passed as arguments, but they are lexically 
% scoped, allowing side effects on any local variable declared in a scope
% outside of the function's scope.
%
% This fact effectively removes any way of determining a Lua function's purity
% outside of compiling a function directly with calls like loadstring and
% dostring, or passing a lua_CFunction function pointer.

	% A refrence to a 
	% This type is identical to the var type, but with a garuntee that
	% the value held by table is, in fact, a Lua table.
	% 
:- type lua.function.

	% Construct a Lua function from a higher order term.
	%
:- pred function((func(state) = list(var))::in, function::out) is det.
:- func function(func(state) = list(var)) = function is det.

	% A typedef for a C function pointer that may be passed to Lua as 
	% a Lua function as defined in the Lua C API.
	%
:- type lua.c_function.

	% Construct a Lua function from a C function pointer.
	%
:- pred c_function(c_function::in, function::out) is det.
:- func c_function(c_function) = function is det.


%-----------------------------------------------------------------------------%

% TODO: coroutines?, iterators?, multi/nondet functions?

%-----------------------------------------------------------------------------%
%
% Userdata
%
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


