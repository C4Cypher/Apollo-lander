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

:- module lua.

:- interface.

:- include_module state.

:- import_module io.
:- import_module float.
:- import_module int.
:- import_module bool.
:- import_module string.
:- import_module char.
:- import_module list.
:- use_module map.
:- import_module univ.
:- import_module require.

%-----------------------------------------------------------------------------%
%
% The Lua State in a pure 'ground' context
%

	% A refrence to the Lua VM as defined by the lua_State type in lua.h
	%
:- type lua.	
	
	% A refrence to the Lua state meant to be passed as a unique value.
	%
:- type lua_state. 

	% Abbriviations for lua_state.

:- type ls == lua_state.

	% Dynamically lookup the value assigned to a variable, will fail
	% if T is not a compatable type for the assigned value.
	% Will not invoke metamethods.
	%
:- pred var(var, T, lua).
:- mode var(in, out, in) is semidet.
:- mode var(out, out, in) is nondet.


:- func var(var, lua) = T.
:- mode var(in, in) = out is semidet.
:- mode var(out, in) = out is nondet.

	% Evaluate a predicate with a locally scoped temporary variable,
	% 
:- pred let(T, pred(var, lua), lua).
:- mode let(in, in(pred(in, in) is det), in) is det.
:- mode let(in, in(pred(in, in) is semidet), in) is semidet.
:- mode let(in, in(pred(in, in) is multi), in) is multi.
:- mode let(in, in(pred(in, in) is nondet), in) is nondet.



%-----------------------------------------------------------------------------%
%
% Imperative Lua via state passing in a 'unique' context.
%

% Due to the fact that the state represented by the lua_State type is mutable, 
% interacting with Lua in a pure context requires that state be unique and
% immutable.
	
	% Retreive the value of a variable in Lua, 
	% the raw version will not invoke metamethods
	% Omitting raw will imply no
	%
:- pred get(var::in, value::out, ls::di, ls::uo) is det.
:- pred rawget(var::in, value::out, ls::di, ls::uo) is det.

	% Assign a value to a variable
	% Raw version will not invoke metamethods
:- pred set(var::in, value::in, ls::di, ls::uo) is det.
:- pred rawset(var::in, value::in, ls::di, ls::uo) is det.

	% Evaluate a function with a pure Lua context
:- pred eval((func(lua) = T), T ls, ls).
:- mode eval((func(in) = out is det), out, di, uo) is det.
:- mode eval((func(in) = out is cc_multi), out, di, uo) is det.

	% Take a Lua state in a pure context and prepare it for stateful calls
	%
:- pred begin(lua::in, io::di, ls::uo) is det.

	% Perform a call requiring io.state using lua_state
:- pred do(pred(io::di, io::uo) is det, ls::di, ls::uo) is det.

	% Finalize the Lua state with return values, and allow no further calls
	% to the Lua state
	%
:- pred return(values::in, ls::in, io::out) is det.

	% Finalize the Lua state to return nil
	%
:- pred end(ls::in, io::out) is det.

	% Call a Lua function
	%
:- pred call(var::in, 

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
:- pred ready(bool::out, ls::di, ls::uo) is det.

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
	;	var(string)	% A named variable
	;	index(value, var)	% Value stored in a table
	;	meta(var)	% A variable's metatable
	
	% The following are meant for internal use
	;	ref(ref)	% A strong refrence (like a pointer)
	;	registry(registry). % A registry entry
	
	% Returned on invalid request.
	;	invalid(string).

:- type vars == list(var).

	% Refers to a value stored in an environment table.
	%
:- func global(string) = var.

% The ref type represents a strong refrence to a Lua variable instantiated in
% Lua, as a result, a refrenced variable will not be garbage collected by Lua
% until said refrence is unregistered or re-assigned.
%
% Note that these refrences discussed here are NOT normal C pointers, but values 
% internal to Lua's register-based VM.  

:- type ref.


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
	;	char(char)	% Passed as string.
	;	chunk(string).	% A chunk of Lua code
	;	lightuserdata(c_pointer)	% naked C pointer
	;	c_userdata(c_pointer)	% fully allocated userdata
	;	thread(lua_state)	% A coroutine
	;	c_function(c_function). % A Lua callable function pointer
	;	var(var)	% A Lua variable
	;	m_userdata(univ)	% Mercury as userdata
	;	lua_error(lua_error). 
	
:- type values == list(value).


:- func value(T) = value is cc_multi.
:- func value_of(value) = T is cc_nondet.


:- some [T] func some_value(value) = T is cc_multi.
	

:- type c_function.	% A Lua callable function defined in C


	
	
:- interface.

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
:- pred register_module(string::in, lua_func::in, lua_state::in,
	io::di, io::uo) is det.


%-----------------------------------------------------------------------------%
%
% Lua functions
%	

	% This is the type signature for predicates that can be cast as
	% Lua functions
	%
:- type lua_func == (impure func(lua_state) = int).


:- func function(lua_func, L) = var.




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
:- func var_type(var, lua) = lua_type.
:- pred var_type(var::in, lua_type::out, ls::di, ls::uo).


	

%-----------------------------------------------------------------------------%
%
% Lua errors
%
	% Thrown when Lua experiences an error.
	%
:- type lua_error
	---> 	lua_error(error_type, string).

:- type error_type
	--->	runtime_error
	;	syntax_error
	;	memory_error
	;	unhandled_error.
	




%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module lua.state.

:- import_module type_desc.
:- import_module int.
:- import_module float.
:- import_module bool.
:- import_module string.
:- import_module require.

:- pragma require_feature_set([conservative_gc, double_prec_float]). 

:- pragma foreign_decl("C", 
"
#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

/* Checking for Lua language features introduced with 5.2 */
#if LUA_VERSION_NUM >= 502

#define AFTER_502

#else /*  LUA_VERSION_NUM < 502 */ 

#define BEFORE_502

#endif /* LUA_VERSION_NUM < 502 */ 

/* Registry values */
#define MR_MODULE ""MR_MODULE""
#define MR_META_UDATA ""MR_UDATA""
#define MR_META_FUNCTION
#define MR_READY ""MR_LUA_IS_READY""

#ifdef BEFORE_502
#define LUA_RIDX_MAINTHREAD     1
#define LUA_RIDX_GLOBALS        2
#define LUA_RIDX_LAST           LUA_RIDX_GLOBALS
#endif /* BEFORE_502 */

/* metatable values*/
#define LUA_META_MR_TYPE ""__mercury_type""






").

	% Succeed if the given value is NULL.
	%
:- pred null(T::in) is semidet.

:- pragma foreign_proc("C", null(T::in),
	[promise_pure, will_not_call_mercury], 
	"SUCCESS_INDICATOR = (T == NULL);").

%-----------------------------------------------------------------------------%
%
% The Lua state
%



% The lua type represents the state of a running Lua Virtual Machine. (Lua VM
% for short) Note that as a convention borrowed from the C API, procedures 
% that query or manipulate the Lua state will use the variable term 'L' to refer 
% to the Lua state.

:- pragma foreign_type("C", lua, "lua_State *",
	[can_pass_as_mercury_type]).

	% This type allows the lua type to be passed uniquely 
	% while being ground for the purpose of passing imperative code.
	% Note, it would be acceptable to interact with lua directly before
	% constructing it into a lua_state, or after, but never both, due to
	% the ambiguity that would bring with Mercury's declarative semantics.
	% I've opted to allow interaction with lua before being passed to
	% stateful operations, or in the scope of a higher order value.
	%
:- type lua_state ---> state(lua).
	
%-----------------------------------------------------------------------------%
%
% Initializing the Lua state
%

:- pragma foreign_proc("C", init_lua(L::in, _I::di, _O::uo), 
	[promise_pure, will_not_call_mercury], "luaMR_init(L);").


:- pragma foreign_proc("C", init_lua(L0::di, L::uo), 
	[promise_pure, will_not_call_mercury], "L0 = L; luaMR_init(L);").


:- pragma foreign_proc("C", ready(L::in), 
	[promise_pure, will_not_call_mercury], "
	SUCCESS_INDICATOR = luaMR_ready(L);
").


:- pragma foreign_proc("C", ready(Answer::out, L0::di, L::uo), 
	[promise_pure, will_not_call_mercury], "
	L0 = L;
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
% Variables
%



:- type index == int.


:- type registry
	--->	global	% Global environment
	;	main_thread	% main thread
	;	modules
	;	meta_userdata
	;	meta_function.
	

%-----------------------------------------------------------------------------%
%
% Refrences
%



:- pragma foreign_type("C", ref, "luaMR_Ref", [can_pass_as_mercury_type]).

:- pragma foreign_code("C",
"
typedef int * luaMR_Ref;


/* Creates a new refrence from the stack */
luaMR_Ref luaMR_new_ref(lua_State * L, int index) {
	lua_pushvalue(L, index);
	luaMR_Ref new_ref = MR_GC_NEW(int);
	*new_ref = luaL_ref(L, LUA_REGISTRYINDEX);
	MR_GC_register_finalizer(new_ref, luaMR_finalize_ref, L);
	return new_ref;
}


/* Push a refrence onto the provided stack */
void luaMR_push_ref(lua_State * L, luaMR_Ref ref) {
	if (*ref == LUA_REFNIL) {
		lua_pushnil(L);
	}
	else {
		lua_rawgeti(L, LUA_REGISTRYINDEX, id);
	}
}

/* Remove Lua's refrence to the var in the registry */
void luaMR_finalize_ref(luaMR_Ref ref, lua_State * L) {
	luaL_unref(L, LUA_REGISTRYINDEX, *ref);
}

"). 


%-----------------------------------------------------------------------------%
%
% Lua values
%

some_value(T) = :-
	( V = nil(U)
	; V = number(U)
	; V = integer(U)
	; V = boolean(U)
	; V = string(U)
	; V = char(U)
	; V = lightuserdata(U)
	; V = thread(U)
	; V = c_function(U)
	; V = var(U)
	; V = var(var(U))
	; V = m_userdata(U)
	) -> dynamic_cast(T, U)
	; V = m_userdata(univ(T)).

value_of(V) = T :- dynamic_cast(V, T).		
		
some_value(V) = T :-
	require_complete_switch [V]
	( V = nil(T)
	; V = number(T)
	; V = number(F),
		(F - truncate_to_int(F)@T) = 0
	; V = integer(T)
	; V = integer(I),
		 T = float(I)
	; V = boolean(T)
	; V = string(T)
	; V = string(S),
		string.length(S) = 1,
		string.det_index(S, 1, T)
	; V = char(T)
	; V = char(C),
		string.from_char(C) = T
	; V = lightuserdata(T)
	; V = c_userdata(T)
	; V = thread(T)
	; V = c_function(T)
	; V = var(T)
	; V = var(var(T))
	; V = m_userdata(T)
	; V = m_userdata(univ(T))
	; V = lua_error(T) ).

%-----------------------------------------------------------------------------%
%
% Length
%

:- pragma foreign_code("C", "

size_t luaMR_len(lua_State *L, int index) {
	
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

register_module(Name, Pred, L, !IO) :-

	( semipure ready(L) ; impure init(L) ),
	 
	( ref(Ref) = function(Pred, L)
		; unexpected($module, $pred, 
		"function/2 did not return a ref.")
	),
	impure lua_getregistry(L, LUA_RIDX_MR_MODULE), /* table -3 */
	impure lua_pushstring(L, Name), /* key -2 */
	impure luaMR_pushref(L, R), /* value -1 */
	impure lua_settable(L, -3), /* table -1 */
	impure lua_pop(L, 1). /* empty stack */
	 
:- pragma promise_pure(register_module/5).


:- pragma foreign_code("C", "

/* take the provided module name and attempt to load an apollo module
passes any additional arguments. */
int luaMR_loader(lua_State * L) {
	if (lua_isstring(L, 1)) {
		const char * module_name = lua_tostring(L, 1);
		luaMR_getregistry(L, LUA_RIDX_MR_MODULE);
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



:- impure func call_pred(lua_state) = int.

call_pred(L) = ReturnCount :-
	semipure Top = lua_gettop(L),
	
	semipure Args = stack_to_list(L, 1, Top),
	
	impure lua_getupvalue(1),
	
	semipure Self = lua_touserdata(L, -1), %TODO
	
	Scope = scope(
		scope(L, Top, 19, map.init), 
		1, 19, map.singleton(var("self"), Self) ),
	
	(try [] Self(Args) = Return 
	then (
		pushlist(L, Return),
		ReturnCount = list.length(Return)
	) catch_any Err -> lua_error(Err) ).
	
:- pragma foreign_export("C", call_pred(in) = out, "luaMR_pred").
	

:- func stack_to_list(lua_state, int, int) = list(value).

stack_to_list(L, Start, End) = 
	[ get(local(Start),L) | 
		( Start > End -> []
		; stack_to_list(L, Start + 1, End) )
	].
			
:- impure pred pushlist(lua_state::in, values::in) is det.

pushlist(L, [V | Vs] ) :-
	 impure lua_push(L, V),
	 impure pushlist(L, Vs).
	 


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


%-----------------------------------------------------------------------------%
%
% Lua errors
%

:- pragma foreign_enum("C", error_type/0,
[
	runtime_error 	-	"LUA_ERRRUN",
	syntax_error 	-	"LUA_ERRSYNTAX",
	memory_error	-	"LUA_ERRMEM",
	unhandled_error	-	"LUA_ERRERR"
]). 


