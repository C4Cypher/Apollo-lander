%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2014 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: luaMR.m.
% Main author: C4Cypher.
% Stability: low.
% 
% This file presents an interface for handling and passing values between
% compiled Mercury modules and the Lua runtime VM.
%
% Lua is known for being a lightweight, easy to write, (comparatively) fast 
% dynamically typed scripting language.  With first-class functions, lexical 
% closures, varadic argument-passing/variable-assignment, it offers a set of
% language features one might expect of a functional language, rather than an 
% imperative scripting language.  With the usage of metatables and a stack
% based C interface, Lua is extremely exstensible and easily embedded or bound
% with other languages.  This flexibility allows Lua programmers to define
% and use their own semantics, be it functional, object-oriented or otherwise.
%
%
% The Semantic gap.
%
% a Lua program can be considered a set of instructions on what to do to.
% These instructions impose changes oand when.In Lua, statements represent 
% imperative changes to the Lua state by producing side effects. In sequential 
% order, Lua evaluates each statement and modifies the Lua state to reflect the 
% truth-value intended by the statement, within the context of the local scope. 
% As such,  Instead of requiring the declaration and deletion of variables, Lua 
% uses 'nil' to represent unnasigned values. 
% 
% In Lua, 'Foo = 3' can be read as 'Foo is now the number 3'.
%
% In contrast to Lua's imperative semantics, Mercury is a purely declarative
% language.  A Mercury program can be considered a set of predicates that
% describe whether or not things are true. Mercury variables aren't containers
% for values that can change, they represent values that Mercury may not know.
% A 'free' variable is one whose value has not yet been determined.
%
% in Mercury, 'Foo = 3' can be read as 'Foo is 3', a statement that can either
% be true or false.
% 
% These are two very different ways of looking at things, and in order to 
% bridge that gap, this Library defines a means of expressing Lua in a given
% context at a specific moment in time.  That context may be the entire Lua
% state of execution, or it could only be the local scope inside a function
% call.  
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module luaMR.


:- interface.

:- include_module state.
:- include_module api.

% Note: The impure operations defined in the api and modules are used to 
% implement this library, however they do not fully conform to the semantics of 
% the prodedures in this library.  Semipure procedures should be safe to be 
% called without special consideration. However; the impure prodedures in the
% api module WILL produce side effects that will produce undefined behavior in
% this library if they are not properly implemented. Use the api module at your
% own risk.

:- import_module io.
:- import_module float.
:- import_module int.
:- import_module bool.
:- import_module string.
:- import_module char.
:- import_module list.
:- use_module map.
:- import_module univ.
:- import_module type_desc.
:- import_module require.

%-----------------------------------------------------------------------------%
%
% The Lua State in an impure context
%

	% A refrence to the Lua VM as defined by the lua_State type in lua.h
	%
:- type lua.


%-----------------------------------------------------------------------------%
%
% The Lua State as a mutable state variable
%

	% A refrence to the Lua state meant to be passed in a safe, 
	% declarative manner. 
	%
:- type lua_state. 	

	% Abbriviations for lua_state.
	%
:- type ls == lua_state.

	% Create a new Lua state.
	%
:- func new_state = lua_state.
:- mode new_state = uo is det.






%-----------------------------------------------------------------------------%
%
% Initializing the Lua state
%

	% Set up the Lua state so that it has all of the assigned values
	% Mercury needs to interact with it.
	%
:- pred init_lua(lua::in, io::di, io::uo) is det.
:- pred init_lua(lua_state::di, lua_state::uo) is det.

	% Check to see if lua_init has been called on a Lua state.
	%
:- semipure pred ready(lua::in) is semidet.

:- pred ready(bool, ls, ls).
:- mode ready(out, di, uo) is det.
:- mode ready(out, mdi, muo) is det.


:- pred ready(ls::mdi, ls::muo) is semidet.

%-----------------------------------------------------------------------------%
%
% Lua variables
%




% A Lua variable can be used to store any value that can be stored as a 
% C type.  Furthermore, because variables are instantiated and stored within
% the Lua state, Mercury cannot construct, deconstruct, equality test or 
% refrence Lua variables directly like it can C types. These operations are
% handled by the C API.

:- type var
	--->	local(index)	% An index on the the local stack
	;	index(value, var)	% Value stored in a table
	;	meta(var)	% A variable's metatable
	
	% The following are meant for internal use
	;	ref(ref)	% A strong refrence (like a pointer)
	;	global(string) 	% A global variable
	
	% Returned on invalid request.
	;	invalid(string).




	% Var ^ T = Var ^ index(value(T)).
	%
:- func var ^ T = var. 

:- type vars == list(var).


% The ref type represents a strong refrence to a Lua variable instantiated in
% Lua, as a result, a refrenced variable will not be garbage collected by Lua
% until said refrence is unregistered or re-assigned.
%
% Note that these refrences discussed here are NOT normal C pointers, but values 
% internal to Lua's register-based VM.  

:- type ref.

%:- func ref(var) = ref.


%-----------------------------------------------------------------------------%
%
% Lua values
%

	% A union of all of the types that can be natively passed to and from
	% Lua.
	%
:- type value
	--->	nil(nil)	% the abscence of value
	;	number(float)	% double prescision, casts to float
	;	integer(int)	% int cast to Lua number
	;	boolean(bool)	% boolean truth values, casts to bool
	;	string(string)	% string value, casts to string
	;	lightuserdata(c_pointer)	% naked C pointer
	;	thread(lua)	% A coroutine
	;	c_function(c_function) % A Lua callable function pointer
	;	var(var)	% A Lua variable
	;	userdata(univ)	% opaque type in Lua for handling foreign data
	;	unbound		% Represents an unbound variable
	;	lua_error(lua_error). 
	
:- inst unbound == bound(unbound).
	
:- type values == list(value).

:- func value(T) = value.
:- mode value(in) = out is det.
:- mode value(unused) = out is det.
:- mode value(out) = in is semidet.

:- func value_of(value) = T.
:- mode value_of(in) = out is semidet.
:- mode value_of(out) = unused is det.
:- mode value_of(out) = in is det.

:- type c_function.	% A Lua callable function defined in C


% The nil value
%
% In Lua, nil represents the abscence of value.  Looking up a key in a Lua table 
% that is not assigned a value with produce a nil result.
%
% Furthermore, assigning a key value to nil will unassign that value. Note that 
% this does not neccicarily delete the value, if Lua holds a refrence to that
% value elsewhere, it will not be garbage collected.
%
% In normal Lua semantics, using nil as a key value produces an error, however
% due to the Mercury semantics used in this library, doing so will either fail
% or return another nil value.  This is both for the sake of safer runtime
% integration of Mercury's strict type system with Lua's dynamic type system,
% and also as a practical consideration of Mercury's potentially
% nondeterministic nature, as testing for a paticular type wil result in a
% backtracking failure.
%
% It is to be noted that Lua's nil value is not to be confused with C's NULL
% value.  While used in similar ways, Lua will interpret C's NULL as the number
% zero, wheras C has no direct representation for Lua's nil value.
%
% As a result of this, Lua's semantics on conditional tests are slightly
% different than C's.   C interprets any numeric value other than 0 as true.
% In contrast, Lua interprets ANY value other than boolean false or nil as true.

:- type nil ---> nil.



%-----------------------------------------------------------------------------%
%
% Lua modules
%	

	% register_module(Module, L, !IO).
	%
	% Register a module in Lua.
	%
%:- pred register_module(string::in, lua_func::in, lua::in,
%	io::di, io::uo) is det.


%-----------------------------------------------------------------------------%
%
% Lua functions
%	

	% This is the type signature for predicates that can be cast as
	% Lua functions
	%
:- type lua_func == (impure func(lua) = int).

:- inst det_lua_func == 
	(func(in) = out(bound(int)) is det).
	
:- inst semidet_lua_func == 
	(func(in) = out is semidet).
	

:- inst dlf == det_lua_func.
:- inst slf == semidet_lua_func.

:- mode dfi == in(det_lua_func).
:- mode dfo == out(det_lua_func).

:- mode sfi == in(semidet_lua_func).
:- mode sfo == out(semidet_lua_func).

:- mode li == in(bound(lua)).
:- mode lo == out(bound(lua)).

:- type func_udata.

:- func func_udata(lua_func) = func_udata.
:- mode func_udata(dfi) = dfuo is det.
:- mode func_udata(sfi) = sfuo is det.
:- mode func_udata(dfo) = dfui is det.
:- mode func_udata(sfo) = sfui is det.


:- type func_udata
	--->	det_func(lua_func)
	;	semidet_func(lua_func).

:- inst dfu ---> det_func(det_lua_func).
:- inst sfu ---> semidet_func(semidet_lua_func).

:- mode dfui == in(dfu).
:- mode dfuo == out(dfu).

:- mode sfui == in(sfu).
:- mode sfuo == out(sfu).


%-----------------------------------------------------------------------------%
%
% Lua types
%	

:- type lua_type
	--->	none			% rarely used, represents invalid type
	;	nil_type		
	;	number_type		
	;	boolean_type		
	;	string_type		
	;	lightuserdata_type	
	;	function_type		
	;	table_type		
	;	thread_type		
	;	userdata_type.		
	
	% Look up the Lua type of a given variable. 
	% 
%:- func var_type(var, lua) = lua_type.
%:- pred var_type(var::in, lua_type::out, ls::di, ls::uo) is det.


	

%-----------------------------------------------------------------------------%
%
% Lua errors
%
	% Thrown when Lua experiences an error.
	%
:- type lua_error
	---> 	lua_error(error_type, string).

:- type error_type
	--->	no_error
	;	runtime_error
	;	syntax_error
	;	memory_error
	;	unhandled_error.
	

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module luaMR.api.
:- import_module luaMR.state.

:- pragma foreign_import_module("C", luaMR.api).
:- pragma foreign_import_module("C", luaMR.state).

:- import_module type_desc.
:- import_module int.
:- import_module float.
:- import_module bool.
:- import_module string.
:- import_module require.


:- pragma require_feature_set([conservative_gc, trailing, double_prec_float]). 

:- pragma foreign_decl("C", 
"
#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

#include <mercury_memory.h>

/* Checking for Lua language features introduced with 5.2 */
#if LUA_VERSION_NUM >= 502

#define AFTER_502

#else /*  LUA_VERSION_NUM < 502 */ 

#define BEFORE_502

#endif /* LUA_VERSION_NUM < 502 */ 

#ifdef BEFORE_502
#define LUA_RIDX_MAINTHREAD     1
#define LUA_RIDX_GLOBALS        2
#define LUA_RIDX_LAST           LUA_RIDX_GLOBALS
#endif /* BEFORE_502 */

#define LUA_MR_MODULES ""LUA_MR_MODULES""
#define LUA_MR_READY ""LUA_MR_READY""

/* metatable values*/
#define LUA_MR_TYPE ""__mercury_type""
#define LUA_MR_USERDATA ""__mercury_userdata""


").



%-----------------------------------------------------------------------------%
%
% The Lua State 
%



% The lua type represents the state of a running Lua Virtual Machine. (Lua VM
% for short) Note that as a convention borrowed from the C API, procedures 
% that query or manipulate the Lua state will use the variable term 'L' to refer 
% to the Lua state.

:- pragma foreign_type("C", lua, "lua_State *",
	[can_pass_as_mercury_type]).

:- pragma foreign_type("C", lua_state, "luaMR_lua_state *").



new_state = lua_state(lua_new, null_id, empty_trail).

%-----------------------------------------------------------------------------%
%
% Initializing the Lua state
%

:- pragma foreign_proc("C", init_lua(L::in, _I::di, _O::uo), 
	[promise_pure, will_not_call_mercury], "luaMR_init(L);").



init_lua(!LS) :- promise_pure( impure init_lua(!.LS ^ lua)).

:- impure pred init_lua(lua::in) is det.

:- pragma foreign_proc("C", init_lua(L::in), 
	[may_call_mercury], "luaMR_init(L);").


:- pragma foreign_proc("C", ready(L::in), 
	[promise_semipure, will_not_call_mercury], "
	SUCCESS_INDICATOR = luaMR_ready(L);
").


ready(lua_state(L, I, T), lua_state(L, I, T)) :- promise_pure (semipure ready(L) ).

ready(B, ls(L, I, T), ls(L, I, T)) :- 
	promise_pure 
	( semipure ready(L) -> B = yes
	; B = no).

:- pragma foreign_decl("C", "void luaMR_init(lua_State *);").

:- pragma foreign_decl("C", "int luaMR_ready(lua_State *);").

:- pragma foreign_code("C", "
void luaMR_init(lua_State * L) {
	
#ifdef BEFORE_502
	
	/* Set the Main thread in the registry */
	lua_pushvalue(L, LUA_REGISTRYINDEX);
	lua_pushinteger(L, LUA_RIDX_MAINTHREAD); 
	if(!lua_pushthread(L))
		MR_fatal_error(""Must init main thread."");
	lua_settable(L, -3);
	
	lua_pushinteger(L, LUA_RIDX_GLOBALS);
	lua_pushvalue(L, LUA_GLOBALSINDEX);
	lua_settable(L, -3);
 
 	
#endif /* BEFORE_502 */

	/* Add tables to the registry. */
	
	lua_newtable(L);
	luaMR_setregistry(L, LUA_MR_MODULES);
	

	/* Add loader to package.loaders */
	lua_getglobal(L, ""package""); 
	lua_getfield(L, -1, ""loaders""); 
	const int length = luaMR_len(L, 1);
	lua_pushinteger(L, length + 1); 
	lua_pushcfunction(L, luaMR_loader); 
	lua_settable(L, -3);
	lua_pop(L, 2);
	
	/* Mark Lua as ready */
	lua_pushboolean(L, 1);
	luaMR_setregistry(L, LUA_MR_READY);
} 

").



:- pragma foreign_code("C", "
	/* Check to see if Lua has already been initialized. */
	int luaMR_ready(lua_State * L) {
		lua_checkstack(L, 1);
		lua_pushvalue(L, LUA_REGISTRYINDEX);
		lua_pushstring(L, LUA_MR_READY);
		lua_gettable(L, -2);
		int ready = lua_toboolean(L, 1);
		lua_remove(L, 1);
		return ready;
	}
").


%-----------------------------------------------------------------------------%
%
% Variables
%



:- type index == int.



	
	
Var ^ T = index(value(T), Var). 

%-----------------------------------------------------------------------------%
%
% Refrences
%



:- pragma foreign_type("C", ref, "luaMR_Ref", [can_pass_as_mercury_type]).

:- pragma foreign_decl("C", "

	typedef int * luaMR_Ref;
	
	luaMR_Ref luaMR_newref(lua_State *, int);
	void luaMR_pushref(lua_State *, luaMR_Ref);
	void luaMR_finalizeref(lua_State *, luaMR_Ref);
").

:- pragma foreign_code("C",
"

/* Creates a new refrence from the stack */
luaMR_Ref luaMR_newref(lua_State * L, int index) {
	lua_pushvalue(L, index);
	luaMR_Ref new_ref = MR_GC_NEW(int);
	*new_ref = luaL_ref(L, LUA_REGISTRYINDEX);
	MR_GC_register_finalizer(new_ref, 
		(GC_finalization_proc)luaMR_finalizeref, L);
	return new_ref;
}


/* Push a refrence onto the provided stack */
void luaMR_pushref(lua_State * L, luaMR_Ref ref) {
	if (*ref == LUA_REFNIL) {
		lua_pushnil(L);
	}
	else {
		lua_rawgeti(L, LUA_REGISTRYINDEX, *ref);
	}
}

/* Remove Lua's refrence to the var in the registry */
void luaMR_finalizeref(lua_State * L, luaMR_Ref ref) {
	luaL_unref(L, LUA_REGISTRYINDEX, *ref);
}

"). 


%-----------------------------------------------------------------------------%
%
% Lua values
%


value(T::in) = (
	( dynamic_cast(T, U:nil) -> nil(U)
	; dynamic_cast(T, U:float) -> number(U)
	; dynamic_cast(T, U:int) -> number(float(U))
	; dynamic_cast(T, U:int) -> integer(U)
	; dynamic_cast(T, U:bool) -> boolean(U)
	; dynamic_cast(T, U:string) -> string(U)
	; dynamic_cast(T, U:char) -> string(string.from_char(U))
	; dynamic_cast(T, U:c_pointer) -> lightuserdata(U)
	; dynamic_cast(T, U:lua) -> thread(U)
	; dynamic_cast(T, U:c_function) -> c_function(U)
	; dynamic_cast(T, U:var) -> var(U)
	; dynamic_cast(T, U:univ) -> userdata(U)
	; userdata(univ(T))
	)::out).
	
value(_::unused) = ((unbound)::out).
	

value(T::out) = (V::in) :-
	require_complete_switch [V]
	( V = nil(N) ->  dynamic_cast(N, T)
	; V = number(F) -> dynamic_cast(F, T)
	; V = integer(I) -> dynamic_cast(I, T)
	; V = boolean(B) -> dynamic_cast(B, T)
	; V = string(S) -> dynamic_cast(S, T)
	; V = lightuserdata(P) -> dynamic_cast(P, T)
	; V = thread(L) -> dynamic_cast(L, T)
	; V = c_function(F) -> dynamic_cast(F, T)
	; V = var(Var) -> dynamic_cast(Var, T)
	; V = userdata(U) -> dynamic_cast(U, T)
	; V = lua_error(E) -> dynamic_cast(E, T)
	; some [Some] V = userdata(univ(Some)) -> dynamic_cast(Some, T)
	; V = unbound, fail
	).

:- pragma promise_pure(value/1).

value_of(V) = T :- value(T) = V.


:- pragma foreign_type("C", c_function, "lua_CFunction").


%-----------------------------------------------------------------------------%
%
% Length
%

:- pragma foreign_decl("C", "
	size_t luaMR_len(lua_State *, int);
").

:- pragma foreign_code("C", "

size_t luaMR_len(lua_State * L, int index) {
	
#ifdef BEFORE_502
	return lua_objlen(L, index);
#else 
	return lua_rawlen(L, index);
#endif /* END BEFORE_502 */
}

").




%-----------------------------------------------------------------------------%
%
% The registry, and upvalues.
%


:- pragma foreign_decl("C", "
	void luaMR_getregistry(lua_State *, const char *);
	void luaMR_setregistry(lua_State *, const char *);
	int  luaMR_getupvalue(lua_State *, const int);
	void luaMR_setupvalue(lua_State *, const int);
").


:- pragma foreign_code("C", "
	void luaMR_getregistry(lua_State * L, const char * k) {
		lua_getfield(L, LUA_REGISTRYINDEX, k);
	}

	void luaMR_setregistry(lua_State * L, const char * k) {
		lua_setfield(L, LUA_REGISTRYINDEX, k);
	}

	int luaMR_getupvalue(lua_State * L, const int id) {
		lua_pushvalue(L, lua_upvalueindex(id));
		if (lua_type(L, -1) == LUA_TNONE) {
			lua_pop(L, 1);
			return 0;
		} else {
			return 1;
		}
	}

	void luaMR_setupvalue(lua_State * L, const int id) {
		lua_replace(L, lua_upvalueindex(id));
	}

").



%-----------------------------------------------------------------------------%
%
% Lua modules
%

/*
register_module(Name, Func, L, !IO) :-

	( semipure ready(L) ; impure init_lua(L, !IO) ),
	 
	( 	impure lua_pushfunc(L, Func),
		impure lua
	; 
		unexpected($module, $pred, 
		"function/2 did not return a ref.")
	),
	impure lua_getregistry(L, LUA_RIDX_MR_MODULE), /* table -3 /
	impure lua_pushstring(L, Name), /* key -2 /
	impure luaMR_pushref(L, R), /* value -1 /
	impure lua_settable(L, -3), /* table -1 /
	impure lua_pop(L, 1). /* empty stack /

:- pragma promise_pure(register_module/5).
*/

:- pragma foreign_decl("C", "
	int luaMR_loader(lua_State *);
").

:- pragma foreign_code("C", "

	/* take the provided module name and attempt to load an apollo module
	passes any additional arguments. */
	int luaMR_loader(lua_State * L) {
		if (lua_isstring(L, 1)) {
			const char * module_name = lua_tostring(L, 1);
			luaMR_getregistry(L, LUA_MR_MODULES);
			lua_getfield(L, 2, module_name);
			return 1;
		}
		return 0;
	}

").


%-----------------------------------------------------------------------------%
%
% Lua functions
%

func_udata(F::dfi) = (U::dfuo) :- U = det_func(F).
func_udata(F::dfo) = (U::dfui) :- U = det_func(F).
func_udata(F::sfi) = (U::sfuo) :- U = semidet_func(F).
func_udata(F::sfo) = (U::sfui) :- U = semidet_func(F).
	
:- pragma promise_pure(func_udata/1).


%-----------------------------------------------------------------------------%
%
% Lua Types
%
:- pragma foreign_enum("C", lua_type/0, 
[
	none 		- 	"LUA_TNONE",
	nil_type 	- 	"LUA_TNIL",
	boolean_type 	- 	"LUA_TBOOLEAN",
	lightuserdata_type - 	"LUA_TLIGHTUSERDATA",
	number_type 	- 	"LUA_TNUMBER",
	string_type 	- 	"LUA_TSTRING",
	table_type 	- 	"LUA_TTABLE",
	function_type 	- 	"LUA_TFUNCTION",
	userdata_type 	- 	"LUA_TUSERDATA",
	thread_type 	- 	"LUA_TTHREAD"
]).

%-----------------------------------------------------------------------------%
%
% Mercury userdata
%

:- pragma foreign_decl("C", "
	MR_Word * luaMR_new(MR_Word);
	int luaMR_free(lua_State *);
").

:- pragma foreign_code("C", "

	MR_Word * luaMR_new(MR_Word word) {
		MR_Word * newptr = MR_GC_malloc_uncollectable(sizeof newptr);
		*newptr = word;
		return newptr; 
	}
	
	int luaMR_free(lua_State * L) {
		MR_Word ** ptr = lua_touserdata(L, 1);
		MR_GC_free(*ptr);
		return 0;
	}
		
").
		
:- impure func to_string(lua) = int.

to_string(L) = 1 :- 
	semipure lua_touserdata(L, 1) = U,
	impure lua_pushstring(L, string.string(univ_value(U))).

		
	
:- pragma foreign_export("C", to_string(in) = out, "luaMR_tostring").



%-----------------------------------------------------------------------------%
%
% Lua errors
%

:- pragma foreign_enum("C", error_type/0,
[
	no_error	-	"0",
	runtime_error 	-	"LUA_ERRRUN",
	syntax_error 	-	"LUA_ERRSYNTAX",
	memory_error	-	"LUA_ERRMEM",
	unhandled_error	-	"LUA_ERRERR"
]). 

